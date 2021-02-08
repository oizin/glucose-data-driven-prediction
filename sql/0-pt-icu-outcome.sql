
-- average number of admissions per subject (1.3)
select avg(n_admissions)
from (
	select subject_id, count(*) as n_admissions
	from mimiciii.admissions a 
	group by subject_id
) tmp

-- average number of ICU stays per subject
select avg(n_admissions)
from (
	select subject_id,count(*) as n_admissions
	from mimiciii.icustays i
	group by subject_id
) tmp

-- average number of ICU stays per stay
select avg(n_admissions)
from (
	select count(*) as n_admissions
	from mimiciii.icustays i
	group by hadm_id 
) tmp


-- Patients ICU outcome (and time to death) -------------------------------

drop materialized view if exists pt_icu_outcome cascade;

create materialized view pt_icu_outcome as

select i.row_id,
	i.subject_id,
	p.dob,
	i.hadm_id,
	a.admittime,
	a.dischtime,
	i.icustay_id,
	(case when 
		(floor(date_part('days',i.intime-dob)/365.25) < 200) then 
		floor(date_part('days',i.intime-dob)/365.25) else 91.4 end) as age_years,
	i.intime,
	i.outtime,
	i.los,
	a.deathtime as hosp_deathtime,
	(case when (a.deathtime >= i.intime and a.deathtime <= i.outtime) 
		then 1 else 0 end) as icu_expire_flag,
	a.hospital_expire_flag,
	p.dod,
	p.expire_flag,
	date_part('days',p.dod - i.intime) as ttd_days
from mimiciii.icustays i left join 
(select subject_id,deathtime, admittime,dischtime,hospital_expire_flag from mimiciii.admissions) a
on i.subject_id = a.subject_id and admittime <= intime and dischtime >= outtime
left join (select subject_id,dob,dod,expire_flag from mimiciii.patients) p on 
i.subject_id = p.subject_id

create unique index icu_outcome_id on pt_icu_outcome(icustay_id);

-- checks
select * from pt_icu_outcome
where age_years > 0

select * from pt_icu_outcome
where icustay_id = 229922

select icustay_id from mimiciii.icustays where first_wardid != last_wardid 



