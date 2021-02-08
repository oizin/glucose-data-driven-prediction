/* Vasopressors

TODO:

*/

drop materialized view if exists vasopressors cascade;

create materialized view vasopressors as

with mv as (
	select icustay_id,
		starttime,
		endtime,
		(case when itemid in (221906) and rate is not null then rate else null end) as Norepinephrine_rate,
		(case when itemid in (221906) and amount is not null then amount else null end) as Norepinephrine_amount,
		(case when itemid in (221289) and rate is not null then rate else null end) as Epinephrine_rate,
		(case when itemid in (221289) and amount is not null then amount else null end) as Epinephrine_amount,
		(case when itemid in (221662) and rate is not null then rate else null end) as Dopamine_rate,
		(case when itemid in (221662) and amount is not null then amount else null end) as Dopamine_amount,
		(case when itemid in (221653) and rate is not null then rate else null end) as Dobutamine_rate,
		(case when itemid in (221653) and amount is not null then amount else null end) as Dobutamine_amount
	from mimiciii.inputevents_mv 
	where itemid in (221906,221289,221662,221653) and statusdescription is distinct from 'Rewritten'
),
cv as (
	select ic.icustay_id,
	ic.charttime as starttime,
	null :: timestamp as endtime,
	max(case
		when itemid = 30047 then rate / coalesce(pw.dailyweight,pw.admissionweight,pw.echoweight,pw.avg_weight_naive,70) -- measured in mcgmin
		when itemid = 30129 then rate
		else null
	end) as Norepinephrine_rate,
	max(case when itemid in (30047,30129) and amount is not null then amount else null end) as Norepinephrine_amount,
	max(case
		when itemid = 30044 then rate / coalesce(pw.dailyweight,pw.admissionweight,pw.echoweight,pw.avg_weight_naive,70)
		when itemid in (30119,30309) then rate -- measured in mcgkgmin
        else null
	end) as Epinephrine_rate,
	max(case when itemid in (30044,30119,30309) and amount is not null then amount else null end) as Epinephrine_amount,
	max(case when itemid in (30043,30307) then rate end) as Dopamine_rate,
	max(case when itemid in (30043,30307) and amount is not null then amount else null end) as Dopamine_amount,
    max(case when itemid in (30042,30306) then rate end) as Dobutamine_rate,
    max(case when itemid in (30042,30306) and amount is not null then amount else null end) as Dopamine_amount
	from mimiciii.inputevents_cv ic
	left join pt_weight pw on ic.icustay_id = pw.icustay_id and pw.starttime <= ic.charttime and pw.endtime >= ic.charttime
	where itemid in (30047,30129,30044,30119,30309,30043,30307,30042,30306)
	group by ic.icustay_id, ic.charttime
	order by ic.icustay_id, ic.charttime
),
mv_cv as (
	select * from mv
	union
	select * from cv
	order by icustay_id,starttime
)
select * from mv_cv;

create index vasopressor_icu_id on vasopressors (icustay_id,starttime,endtime);

----- checks etc ---------------

-- join on hour and day information (potential view)
with tmp as ( 
	select v.*,psh.hr,psh.dy
	from vasopressors v
	left join pt_stay_hr psh
	on v.icustay_id = psh.icustay_id and 
	v.starttime >= psh.starttime and 
	v.starttime < psh.endtime
	order by v.icustay_id, v.starttime
)
select tmp.*, pw.avg_weight_naive
from tmp left join 
pt_weight pw 
on tmp.icustay_id = pw.icustay_id

-- join on hour and day information and further summarise
with tmp as ( 
	select v.*,psh.hr,psh.dy
	from vasopressors v
	left join pt_stay_hr psh
	on v.icustay_id = psh.icustay_id and 
	v.starttime >= psh.starttime and 
	v.starttime < psh.endtime
	order by v.icustay_id, v.starttime
),
tmp1 as (
	select tmp.*, pw.avg_weight_naive
	from tmp left join 
	pt_weight pw 
	on tmp.icustay_id = pw.icustay_id
)
select icustay_id,hr,dy,
	avg(norepinephrine_rate) as norepinephrine_rate,
	avg(epinephrine_rate) as epinephrine_rate,
	avg(dopamine_rate) as dopamine_rate,
	avg(dobutamine_rate) as dobutamine_rate
from tmp1
group by icustay_id,hr,dy;

	
--- reasonableness of dosages
select dopamine_rate,count(*) as n
from vasopressors v
where dopamine_rate is not null
group by dopamine_rate
order by n desc

	
	