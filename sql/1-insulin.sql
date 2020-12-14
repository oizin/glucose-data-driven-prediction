/* Insulin and glucose
 * 
 * Contents:
 * 1. Insulin from metavision
 * 2. Insulin joined to patient hour stay table
 * 3. Insulin joined to the vital signs
 * 4. Insulin+vitals+hr/dy
 *
 * Sources:
 * https://github.com/MIT-LCP/mimic-code/pull/635/files#diff-65fcc637b9ee380f62b5a581c7dc024d
 *
-- CAREVUE not used:
--5228	30310	Insulin Drip		carevue	inputevents_cv
--5366	30045	Insulin		carevue	inputevents_cv
--5402	30100	Regular Insulin		carevue	inputevents_cv
--8292	44518	Insulin Carrier		carevue	inputevents_cv
--9620	45186	Insulin carrier		carevue	inputevents_cv
--9724	45322	NS Insulin Carrier		carevue	inputevents_cv
 *
 *
*/

-- 1. Insulin from metavision only
drop materialized view if exists insulin_mv cascade;

create materialized view insulin_mv as

select icustay_id,
	subject_id, 
	hadm_id,
	cast(starttime as timestamp) as starttime,
	cast(endtime as timestamp) as endtime,
	(case when itemid in (223257,223258,223259,223260,223261,223262) and rate is not null then rate else null end) as insulin_rate,
	originalrate,
	rateuom,
	(case when itemid in (223257,223258,223259,223260,223261,223262) and amount is not null then amount else null end) as insulin_amount,
	amountuom,
	ordercategoryname ,
	ordercategorydescription,
	(case
        when itemid=223257 then 'Intermediate' --'Ins7030'
        when itemid=223258 then 'Short'        --'InsRegular'
        when itemid=223259 then 'Intermediate' --'InsNPH'
        when itemid=223260 then 'Long'         --'InsGlargine'
        when itemid=223261 then 'Intermediate' --'InsHum7525'
        when itemid=223262 then 'Short'        --'InsHum'
        else null 
 	end) AS insulin_type,
	(case
	    when upper(ORDERCATEGORYNAME) LIKE '%NON IV%' THEN 'BOLUS_INJECTION'
	    when upper(ORDERCATEGORYNAME) LIKE '%MED BOLUS%' THEN 'BOLUS_PUSH'
	    when ORDERCATEGORYNAME IN ('01-Drips','12-Parenteral Nutrition') THEN 'INFUSION'
 	ELSE null END) AS insulin_admin,
	(CASE WHEN STATUSDESCRIPTION IN ('Paused','Stopped') THEN 1 ELSE NULL END) AS inf_stop,
	patientweight,
	cast(storetime as timestamp) as storetime
from mimiciii.inputevents_mv 
where itemid in (223257,223258,223259,223260,223261,223262) and 
	statusdescription is distinct from 'Rewritten'
order by icustay_id, subject_id, hadm_id, starttime;


-- 2. Insulin joined to patient hour stay table
drop materialized view if exists insulin_mv_hr_dy cascade;

create materialized view insulin_mv_hr_dy as

select im.*,psh.hr,psh.dy
from insulin_mv im
left join pt_stay_hr psh
on im.icustay_id = psh.icustay_id and 
im.starttime >= psh.starttime and 
im.starttime < psh.endtime
order by im.icustay_id, im.starttime;


----- checks, tests etc -----------------------------------------------------
--select * from insulin_mv order by icustay_id, starttime;
--select * from insulin_mv_hr_dy order by icustay_id, starttime;
--select * from mimiciii.d_items di where label like '%Insulin%' and dbsource ='carevue';
--select * from mimiciii.d_items di where label like '%Insulin%' and dbsource ='metavision';

-- 3. Glucose + Insulin (METAVISION) -----------------------------------------------------------

drop materialized view if exists glycaemic_analysis_t cascade;

create materialized view glycaemic_analysis_t as

with glucose_labs as (
	select pts.icustay_id, 
		pl.*
	from pv_labs pl left join 
		(select distinct icustay_id,hadm_id from pt_stay_dy) pts on
	pts.hadm_id = pl.hadm_id
),
insulin_vitals as (
	select coalesce(pv.icustay_id,i.icustay_id) as icustay_id,
		coalesce(pv.charttime,i.starttime) as time,
		pv.charttime,
		pv.glucose as glucose_b,
		pv.bilirubin,
		pv.diasbp,
		pv.fio2,
		pv.glucose,
		pv.heartrate,
		pv.meanarterialpressure,
		pv.resprate,
		pv.spo2,
		pv.sysbp,
		pv.temperature,
		i.patientweight,
		i.starttime as med_starttime,
		i.endtime as med_endtime,
		i.storetime,
		i.insulin_rate,
		i.originalrate,
		i.rateuom,
		i.insulin_amount,
		i.amountuom,
		i.insulin_type,
		i.insulin_admin,
		i.inf_stop,
		i.ordercategoryname,
		i.ordercategorydescription
	from 
		pv_vitals pv 
		full join insulin_mv_hr_dy i on
		i.icustay_id = pv.icustay_id and
		pv.charttime = i.starttime
)
select coalesce(iv.icustay_id,gl.icustay_id) as icustay_id,
		coalesce(iv.time,gl.charttime) as time,
		glucose_b,
		gl.glucose as glucose_l,
		iv.bilirubin as bilirubin_b,
		iv.diasbp,
		iv.fio2 as fio2_b,
		iv.heartrate,
		iv.meanarterialpressure,
		iv.resprate,
		iv.spo2,
		iv.sysbp,
		iv.temperature,
		iv.patientweight,
		gl.neutrophil,
		gl.creactiveprotein,
		gl.whitebloodcell,
		gl.partialpressureo2,
		gl.bicarbonate,
		gl.lactate,
		gl.troponin,
		gl.bloodureanitrogen,
		gl.creatinine,
		gl.alaninetransaminase,
		gl.aspartatetransaminase,
		gl.hemoglobin,
		gl.intnormalisedratio,
		gl.platelets,
		gl.albumin,
		gl.chloride,
		gl.glucose,
		gl.sodium,
		gl.bilirubin as bilirubin_l,
		gl.hematocrit,
		gl.fio2 as fio2_l,
		iv.med_starttime,
		iv.med_endtime,
		iv.storetime,
		iv.insulin_rate,
		iv.originalrate,
		iv.rateuom,
		iv.insulin_amount,
		iv.amountuom,
		iv.insulin_type,
		iv.insulin_admin,
		iv.inf_stop,
		iv.ordercategoryname,
		iv.ordercategorydescription
from insulin_vitals iv full join glucose_labs gl on 
	iv.icustay_id = gl.icustay_id and 
	iv.time = gl.charttime
order by icustay_id, time;

-- checks, tests, etc ------------------------------------------------
select * from glycaemic_analysis_hr;
select count(*) from glycaemic_analysis_t;
--select * from pv_vitals
--select * from pt_stay_hr

-- 4. join to pt_stay_hr_dy -----------------------------------------

drop materialized view if exists glycaemic_analysis_hr cascade;

create materialized view glycaemic_analysis_hr as

select g.*,
	p.hadm_id,
	p.subject_id,
	p.hr,
	p.dy,
	p.intime,
	p.outtime,
	p.starttime as hr_startime,
	p.endtime as hr_endtime
from glycaemic_analysis_t g
left join 
pt_stay_hr p on 
	g.icustay_id = p.icustay_id and 
	g.time >= p.starttime and 
	g.time < p.endtime 
where hr is not null

-- checks, tests, etc ------------------------------------------------
--select * from glycaemic_analysis_hr  
select count(*) from glycaemic_analysis_hr
--select * from pv_vitals
--select * from pt_stay_hr
