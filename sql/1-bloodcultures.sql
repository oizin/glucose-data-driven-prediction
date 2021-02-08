/* Blood culture results

TODO:

*/

drop materialized view if exists bloodculture cascade;
create materialized view bloodculture as
 
with bc as (
	select hadm_id,
	    chartdate, 
	    charttime,
	    spec_type_desc,
	    org_name,
	    -- if organism is present, then this is a positive culture, otherwise it's negative
	    (case when org_name is not null and org_name != '' then 1 else 0 end) as PositiveCulture,
	    ab_name,
	    interpretation as antibioticResistance
	from mimiciii.microbiologyevents
),
bc_dy as (
	select bc.hadm_id,psh.icustay_id,psh.dy,bc.charttime,bc.chartdate,bc.org_name,bc.positiveculture,bc.ab_name,bc.antibioticResistance
	from bc left join pt_stay_dy psh on
	bc.hadm_id = psh.hadm_id and 
	psh.starttime >= bc.chartdate and 
	(psh.starttime - interval '1' day) < bc.chartdate
),
bc_hr_dy as (
	select bc.hadm_id,bc.icustay_id,bc.dy,psh.hr,bc.charttime,bc.chartdate,bc.org_name,bc.positiveculture,bc.ab_name,bc.antibioticResistance
	from bc_dy bc left join pt_stay_hr psh on
	bc.hadm_id = psh.hadm_id and 
	psh.starttime <= bc.charttime and 
	psh.endtime >= bc.charttime
)

select * from bc_hr_dy;

create index culture_icu_id on bloodculture (icustay_id,charttime,hr);

----- checks etc ---------------

-- query
select *
from bloodculture
where dy = 0;

-- a simplifying query
select icustay_id,dy,hr,max(positiveculture) as positiveculture
from bloodculture
where icustay_id is not null
group by icustay_id,dy,hr; 

----- end -------------------------------------------------------------------
