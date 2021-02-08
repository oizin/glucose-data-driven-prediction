/*
Quick SOFA

*/

drop table if exists qsofa cascade;

create table qsofa as

with co_stg as (
	select icustay_id,
	hadm_id,
	date_trunc('hour',intime) as intime,
	outtime,
	generate_series (
		-24,
		ceil(extract(epoch from outtime-intime)/60.0/60.0)::INTEGER
	) as hr
	from mimiciii.icustays ie
	inner join mimiciii.patients pt
	on ie.subject_id = pt.subject_id
	where ie.intime > (pt.dob + interval '1' year)
),
co as (
	select icustay_id, hadm_id, intime, outtime,
	hr*(interval '1' hour) + intime - interval '1' hour as starttime,
	hr*(interval '1' hour) + intime as endtime,
	hr
	from co_stg
),
bp_rr as (
	select icustay_id,charttime,sysbp,resprate
	from pv_vitals
),
bp_rr_co as (
	select co.icustay_id,co.hr,
		min(sysbp) as sysBP,
		max(resprate) as respRate
	from co 
	left join bp_rr
	on co.icustay_id = bp_rr.icustay_id and
	bp_rr.charttime >= co.starttime and
	bp_rr.charttime <= co.endtime
	group by co.icustay_id, co.hr
),
gcs as (
	select icustay_id,charttime,gcs
	from pv_gcs
),
gcs_co as (
	select co.icustay_id,co.hr,
	min(gcs) as gcs
	from co
	left join gcs
	on co.icustay_id = gcs.icustay_id and
	gcs.charttime >= co.starttime and
	gcs.charttime <= co.endtime
	group by co.icustay_id,co.hr
),
scorecomp as (
	select co.icustay_id,
		co.hr,
		co.starttime,
		co.endtime,
		min(sysbp) as sysBP,
		max(resprate) as respRate,
		min(gcs) as gcs
	from co 
	left join bp_rr_co
	on co.icustay_id = bp_rr_co.icustay_id and
	co.hr = bp_rr_co.hr
	left join gcs_co
	on co.icustay_id = gcs_co.icustay_id and
	co.hr = gcs_co.hr
	group by co.icustay_id,co.hr,co.starttime,co.endtime
	order by co.icustay_id,co.hr,co.starttime,co.endtime
)
select scorecomp.*,
((case when sysBP <= 100 then 1 else 0 end) +
(case when resprate >= 22 then 1 else 0 end) +
(case when gcs <= 14 then 1 else 0 end))::SMALLINT as qsofa,
(case when sysBP is null then 1 else 0 end) +
(case when resprate is null then 1 else 0 end) +
(case when gcs is null then 1 else 0 end) as missingflag
from scorecomp
where hr >= 0
order by icustay_id, hr;
