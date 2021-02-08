/*
Create a pivoted view of a selection of item codes from the labevents table.
Variables
- Neutrophil
- C Reactive Protein
- White Blood Cell count
- Partial Pressure O2 (PO2)
- Bicarbonate
- Lactate
- Troponin
- Blood Urea Nitrogen
- Creatinine
- Alanine Transaminase
- Aspartate Transaminase
- Hemoglobin
- International Normalised Ratio
- Platelet count
- Albumin
- Chloride
- Glucose
- Sodium
- Bilirubin: an orange-yellow substance made during the normal breakdown of red blood cells
- Hematocrit

TODO
- add error ranges to case when select statements (e.g. if neutrophil count is 99998 then null)

source: 
https://github.com/MIT-LCP/mimic-code/blob/ddd4557423c6b0505be9b53d230863ef1ea78120/concepts/pivot/pivoted-lab.sql
https://github.com/MIT-LCP/mimic-code/blob/ddd4557423c6b0505be9b53d230863ef1ea78120/concepts/pivot/pivoted-bg.sql

Running time: 1.5 min
*/

drop materialized view if exists pv_labs cascade;

create materialized view pv_labs as

with labs as
(
	select le.hadm_id, le.charttime,
	(case when itemid in (51256) then valuenum else null end) as neutrophil,
	(case when itemid in (50889) then valuenum else null end) as cReactiveProtein,
	(case when itemid in (51300,51301) then valuenum else null end) as whiteBloodCell,
	(case when itemid in (50821) then valuenum else null end) as partialPressureO2,
	(case when itemid in (50882,50803) then valuenum else null end) as bicarbonate,
	(case when itemid in (50813) then valuenum else null end) as lactate,
	(case when itemid in (51003) then valuenum else null end) as troponin,
	(case when itemid in (51006) then valuenum else null end) as bloodUreaNitrogen,
	(case when itemid in (50912) then valuenum else null end) as creatinine,
	(case when itemid in (50861) then valuenum else null end) as alanineTransaminase,
	(case when itemid in (50878) then valuenum else null end) as aspartateTransaminase,
	(case when itemid in (50811,51222) then valuenum else null end) as hemoglobin,
	(case when itemid in (51237) then valuenum else null end) as intNormalisedRatio,
	(case when itemid in (51265) then valuenum else null end) as platelets,
	(case when itemid in (50862) then valuenum else null end) as albumin,
	(case when itemid in (50806,50902) then valuenum else null end) as chloride,
	(case when itemid in (50809,50931) then valuenum else null end) as glucose,
	(case when itemid in (50924,50983) then valuenum else null end) as sodium,
	(case when itemid in (50885) and valuenum < 150 then valuenum else null end) as bilirubin,
	(case when itemid in (51221) then valuenum else null end) as hematocrit,
	(case when itemid in (50816) and valuenum > 20 then valuenum else null end) as fio2
	from mimiciii.labevents le
	where le.itemid in (
		51256,
		50889,
		51300,51301,
		50821,
		50882,50803,
		50813,
		51003,
		51006,
		50912,
		50861,
		50878,
		50811,51222,
		51237,
		51265,
		50862,
		50806,50902,
		50809,50931,
		50924,50983,
		50885,
		51221,
		50816
	)
)

select labs.hadm_id,
labs.charttime,
avg(neutrophil) as neutrophil,
avg(cReactiveProtein) as cReactiveProtein,
avg(whiteBloodCell) as whiteBloodCell,
avg(partialPressureO2) as partialPressureO2,
avg(bicarbonate) as bicarbonate,
avg(lactate) as lactate,
avg(troponin) as troponin,
avg(bloodUreaNitrogen) as bloodUreaNitrogen,
avg(creatinine) as creatinine,
avg(alanineTransaminase) as alanineTransaminase,
avg(aspartateTransaminase) as aspartateTransaminase,
avg(hemoglobin) as hemoglobin,
avg(intNormalisedRatio) as intNormalisedRatio,
avg(platelets) as platelets,
avg(albumin) as albumin,
avg(chloride) as chloride,
avg(glucose) as glucose,
avg(sodium) as sodium,
avg(bilirubin) as bilirubin,
avg(hematocrit) as hematocrit,
avg(fio2) as fio2
from labs 
group by labs.hadm_id, labs.charttime
order by labs.hadm_id, labs.charttime;

create unique index labs_hosp_id on pv_labs(hadm_id,charttime);
