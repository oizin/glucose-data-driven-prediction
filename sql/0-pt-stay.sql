/*
Patient timeline information: time of ICU entry and exit, broken down by (patient) day
- Time of ICU entry and exit
- Day 0 is one day prior to ICU entry

*/

-- Patient by day -------------------------------------------------------
drop materialized view if exists pt_stay_dy cascade;

create materialized view pt_stay_dy as

with co_stg as (
	select icustay_id,
	hadm_id,
	intime,
	outtime,
	generate_series (
		0,
		ceil(extract(epoch from outtime-intime)/60.0/60.0/24.0)::INTEGER
	) as dy
	from mimiciii.icustays ie
	inner join mimiciii.patients pt
	on ie.subject_id = pt.subject_id
	-- where ie.intime > (pt.dob + interval '1' year) -- remove neonates
),
co as (
	select icustay_id, hadm_id, intime, outtime,
	dy*(interval '1' day) + intime - interval '1' day as starttime,
	dy*(interval '1' day) + intime as endtime,
	dy
	from co_stg
)

select *
from co
order by icustay_id,starttime;

-- Patient by hour: 8,830,412 rows ---------------------------------------
drop materialized view if exists pt_stay_hr cascade;

create materialized view pt_stay_hr as

with co_stg as (
	select icustay_id,
	hadm_id,
	ie.subject_id,
	intime,
	outtime,
	generate_series (
		-24,
		ceil(extract(epoch from outtime-intime)/60.0/60.0)::INTEGER
	) as hr
	from mimiciii.icustays ie
	inner join mimiciii.patients pt
	on ie.subject_id = pt.subject_id
	-- where ie.intime > (pt.dob + interval '1' year) -- remove neonates
),
co as (
	select icustay_id, hadm_id, subject_id, intime, outtime,
	hr*(interval '1' hour) + intime as starttime,
	hr*(interval '1' hour) + intime + interval '1' hour as endtime,
	hr
	from co_stg
),
co_dy as (
	select co.*,psd.dy
	from co 
	left join pt_stay_dy psd 
	on co.icustay_id = psd.icustay_id and
	co.starttime >= psd.starttime and 
	co.endtime <= psd.endtime
)

select *
from co_dy
order by icustay_id,starttime;

create unique index icu_id on pt_stay_hr (icustay_id,hr);
create unique index hosp_id on pt_stay_hr (hadm_id,icustay_id,hr);

--- checks etc ------------
select * from pt_stay_hr
select * from mimiciii.patients 
