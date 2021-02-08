/* 
Create a pivoted view of urine output


source: 
https://github.com/MIT-LCP/mimic-code/blob/ddd4557423c6b0505be9b53d230863ef1ea78120/concepts/pivot/pivoted-uo.sql

Running time: 30s
*/

drop materialized view if exists pv_urine cascade;
create materialized view pv_urine as

with urine as (
	select oe.icustay_id, 
	oe.charttime,
	case when oe.itemid = 227488 and oe.value > 0 then -1*oe.value
		else oe.value
	end as UrineOutput
	from mimiciii.outputevents oe
	where oe.iserror is distinct from 1
	and itemid in 
	(
		-- Metavision
		40055, -- "Urine Out Foley"
		43175, -- "Urine ."
		40069, -- "Urine Out Void"
		40094, -- "Urine Out Condom Cath"
		40715, -- "Urine Out Suprapubic"
		40473, -- "Urine Out IleoConduit"
		40085, -- "Urine Out Incontinent"
		40057, -- "Urine Out Rt Nephrostomy"
		40056, -- "Urine Out Lt Nephrostomy"
		40405, -- "Urine Out Other"
		40428, -- "Urine Out Straight Cath"
		40086,--	Urine Out Incontinent
		40096, -- "Urine Out Ureteral Stent #1"
		40651, -- "Urine Out Ureteral Stent #2"
		-- CareVue
		226559, -- "Foley"
  		226560, -- "Void"
  		226561, -- "Condom Cath"
  		226584, -- "Ileoconduit"
  		226563, -- "Suprapubic"
  		226564, -- "R Nephrostomy"
  		226565, -- "L Nephrostomy"
  		226567, --	Straight Cath
  		226557, -- R Ureteral Stent
  		226558, -- L Ureteral Stent
  		227488, -- GU Irrigant Volume In
  		227489  -- GU Irrigant/Urine Volume Out
	)
)
select icustay_id,
charttime,
sum(UrineOutput) as UrineOutput
from urine
group by icustay_id,charttime
order by icustay_id,charttime;

create unique index uo_icu_id on pv_urine(icustay_id,charttime);