/*
Create a pivoted view of the Glasgow Coma Score components

The Glasgow coma scale is used to assess patients in a coma. The initial score correlates with the 
severity of brain injury and prognosis. The Glasgow Coma Scale provides a score in the range 3-15; 
patients with scores of 3-8 are usually said to be in a coma.

There are three components:

- Eye response (E)
There are four grades starting with the most severe:
    1. No opening of the eye
    2. Eye opening in response to pain stimulus. (a peripheral pain stimulus, such as squeezing the lunula area of the person's fingernail 
    is more effective than a central stimulus such as a trapezius squeeze, due to a grimacing effect).[5]
    3. Eye opening to speech. (Not to be confused with the awakening of a sleeping person; such people receive a score of 4, not 3.)
    4. Eyes opening spontaneously

- Verbal response (V)
There are five grades starting with the most severe:
    1. No verbal response
    2. Incomprehensible sounds. (Moaning but no words.)
    3. Inappropriate words. (Random or exclamatory articulated speech, but no conversational exchange. Speaks words but no sentences.)
    4. Confused. (The person responds to questions coherently but there is some disorientation and confusion.)
    5. Oriented. (Person responds coherently and appropriately to questions such as the person’s name and age, where they are and why, the year, month, etc.)

- Motor response (M)
There are six grades:
    1. No motor response
    2. Decerebrate posturing accentuated by pain (extensor response: adduction of arm, internal rotation of shoulder, pronation of forearm and extension at 
    elbow, flexion of wrist and fingers, leg extension, plantarflexion of foot)
    3. Decorticate posturing accentuated by pain (flexor response: internal rotation of shoulder, flexion of forearm and wrist with clenched fist, leg extension, p
    lantarflexion of foot)
    4. Withdrawal from pain (absence of abnormal posturing; unable to lift hand past chin with supraorbital pain but does pull away when nailbed is pinched)
    5. Localizes to pain (purposeful movements towards painful stimuli; e.g., brings hand up beyond chin when supraorbital pressure applied)
    6. Obeys commands (the person does simple things as asked)

GCS = M + V + E

source: wikipedia
https://github.com/MIT-LCP/mimic-code/blob/ddd4557423c6b0505be9b53d230863ef1ea78120/concepts/pivot/pivoted-gcs.sql
*/

drop materialized view if exists pv_gcs cascade;
create materialized view pv_gcs as
with base as 
(
	select ce.icustay_id, ce.charttime,
	max(case when ce.itemid in (454,223901) then ce.valuenum else null end) as GCSMotor,
	max(case when ce.itemid in (723,223900) then ce.valuenum else null end) as GCSVerbal,
	max(case when ce.itemid in (184,220739) then ce.valuenum else null end) as GCSEyes,
	max(case 
		when ce.itemid = 723 and ce.value = '1.0 ET/Trach' then 1
		when ce.itemid = 223900 and ce.value = 'No Response-ETT' then 1
		else 0 end
		) as endotrachflag,
	row_number () over (partition by ce.icustay_id order by ce.charttime asc) as rn
	from mimiciii.chartevents ce
	where ce.itemid in (
	184,454,723,
	223900,223901,220739
	)
	and ce.error is distinct from 1
	group by ce.icustay_id, ce.charttime
),
gcs as 
(
	select b.*,
	b2.GCSVerbal as GCSVerbalPrev,
	b2.GCSMotor as GCSMotorPrev,
	b2.GCSEyes as GCSEyesPrev,
	case 
	when b.GCSVerbal = 0
		then 15
	when b.GCSVerbal is null and b2.GCSVerbal = 0
		then 15
	when b2.GCSVerbal = 0
		then coalesce(b.GCSMotor,6)+coalesce(b.GCSVerbal,5)+coalesce(b.GCSEyes,4)
	else coalesce(b.GCSMotor,coalesce(b2.GCSMotor,6))+
		coalesce(b.GCSVerbal,coalesce(b2.GCSVerbal,5))+
			coalesce(b.GCSEyes,coalesce(b2.GCSEyes,4))
	end as GCS
	from base b
	left join base b2
		on b.icustay_id = b2.icustay_id
		and b.rn = b2.rn+1
		and b2.charttime > b.charttime - interval '6' hour
),
gcs_stg as
(
	select gs.icustay_id, gs.charttime,
	GCS,
	coalesce(GCSMotor,GCSMotorPrev) as GCSMotor,
	coalesce(GCSVerbal,GCSVerbalPrev) as GCSVerbal,
	coalesce(GCSEyes,GCSEyesPrev) as GCSEyes,
	case when coalesce(GCSMotor,GCSMotorPrev) is null then 0 else 1 end +
	case when coalesce(GCSVerbal,GCSVerbalPrev) is null then 0 else 1 end +
	case when coalesce(GCSEyes,GCSEyesPrev) is null then 0 else 1 end
		as components_measured,
	EndoTrachFlag
	from gcs gs
),
gcs_priority as
(
	select icustay_id,
	charttime,
	GCS,
	GCSMotor,
	GCSVerbal,
	GCSEyes,
	EndoTrachFlag,
	row_number () over (
		partition by icustay_id, charttime
		order by components_measured desc, endotrachflag, gcs, charttime desc
	) as rn
	from gcs_stg
)
select icustay_id,
charttime,
GCS,
GCSMotor,
GCSVerbal,
GCSEyes,
EndoTrachFlag
from gcs_priority gs
where rn = 1
order by icustay_id,charttime;

create unique index gcs_icu_id on pv_gcs (icustay_id,charttime);
