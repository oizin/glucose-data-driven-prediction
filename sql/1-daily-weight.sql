/*
Daily patient weight
From several sources - admission weight, echo data, daily weight
Up to 10% of patients have missing weight

Running time: 1min
*/

drop materialized view if exists pt_weight cascade;

create materialized view pt_weight as

with 
wt as
(
	select ce.icustay_id, 
	ce.charttime,
	(case 
		when itemid in (226512,762) then valuenum
		when itemid in (226531) then valuenum*0.453592
		else null
	end) as AdmissionWeight,
	(case when itemid in (763,224639,2582,3580) then valuenum 
	 WHEN itemid IN (3581) THEN valuenum*0.45359237 
	 WHEN itemid IN (3582) THEN valuenum*0.0283495231 
	 else null 
	 end) as DailyWeight,
	 (case when itemid in (580,581) then valuenum
	 else null
	 end) as PreviousWeight
	from mimiciii.chartevents ce
	where itemid in (
		580,581,  -- Previous weight
		2582,3580,3581,3582,  -- Present weight
		226512,762,226531,  -- Admit weight
		763,224639  -- Daily weight
	) and ce.error is distinct from 1
	order by icustay_id, charttime
),
echo_wt as (
	select ie.icustay_id, avg(weight * 0.45359237) as EchoWeight
  	from mimiciii.icustays ie
  	left join echodata echo  -- a view created in echodata.sql
    	on ie.hadm_id = echo.hadm_id
    	and echo.charttime > ie.intime - interval '7' day
    	and echo.charttime < ie.intime + interval '7' day
  	group by ie.icustay_id
),
pt_weight as (
	select psd.icustay_id, psd.dy, psd.starttime, psd.endtime,
		avg(AdmissionWeight) as Admissionweight,
		avg(DailyWeight) as DailyWeight,
		avg(PreviousWeight) as PreviousWeight
	from pt_stay_dy psd
	left join wt
	on psd.icustay_id = wt.icustay_id and wt.charttime >= psd.starttime and wt.charttime <= psd.endtime
	group by psd.icustay_id, psd.dy,psd.starttime, psd.endtime
	order by psd.icustay_id, psd.dy
),
pt_weight_echo as (
	select pw.*,ew.EchoWeight
	from pt_weight pw left join echo_wt ew
	on pw.icustay_id = ew.icustay_id
	order by pw.icustay_id, pw.dy
),
av_weight as (
	select icustay_id, 
	avg(coalesce(admissionweight,dailyweight,echoweight)) as avg_weight_naive,
	min(coalesce(admissionweight,dailyweight,echoweight)) as min_weight,
	max(coalesce(admissionweight,dailyweight,echoweight)) as max_weight 
	from pt_weight_echo 
	group by icustay_id
)

select pwe.*, aw.avg_weight_naive, aw.min_weight, aw.max_weight
from pt_weight_echo	pwe
left join av_weight aw
on pwe.icustay_id = aw.icustay_id
order by icustay_id, dy;


------- checks etc -----------------
select * from pt_weight
-- patients with no weight


