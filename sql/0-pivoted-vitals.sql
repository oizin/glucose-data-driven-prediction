/*
Create a pivoted table of variable from the mimiciii.chartevents table.
Variables of interest:
- FiO2: Fraction of inspired oxygen, Natural air includes 21% oxygen, which is equivalent to FiO2 of 0.21
- SpO2: Peripheral capillary oxygen saturation, an estimate of the amount of oxygen in the blood. Percentage 
	of oxygenated haemoglobin (haemoglobin containing oxygen) compared to the total amount of haemoglobin in the 
	blood ( oxygenated and non-oxygenated haemoglobin).
- temperature
- oxygen saturation: peripheral capillary oxygen saturation
- respiratory rate
- heart rate
- systolic blood pressure
- diastolic blood pressure
- mean arterial pressure
- blood glucose

TODO
- add error ranges to case when select statements (e.g. if HR is 0 then null)

Running time: 4 min
*/

drop materialized view if exists pv_vitals;
create materialized view pv_vitals as
with ce as
(
	select ce.icustay_id, 
	ce.charttime,
	(case 
		when itemid = 223835
			then case 
				when valuenum > 0 and valuenum <= 1
					then valuenum * 100 -- improperly input data - looks like O2 flow in litres
          		 when valuenum > 1 and valuenum < 21
		    		then null
				when valuenum >= 21 and valuenum <= 100
					then valuenum
					else null 
		end 
		when itemid in (3420, 3422) then valuenum
	    when itemid = 190 and valuenum > 0.20 and valuenum < 1     
            then valuenum * 100
	    	else null 
		end) as fio2,
	(case when itemid in (646,220277) and valuenum >= 0 or valuenum < 100 then valuenum else null end) as spO2,
	(case when itemid in (223761,678) then (valuenum-32)/1.8 -- convert fahrenheit to celcius
		when itemid in (223762,676) then valuenum else null end) as temperature,
	(case when itemid in (615,618,220210,224690) then valuenum else null end) as respRate,
	(case when itemid in (211,220045) then valuenum else null end) as heartRate,
	(case when itemid in (51,442,455,6701,220179,220050) then valuenum else null end) as sysBP,
	(case when itemid in (8368,8440,8441,8555,220180,220051) then valuenum else null end) as diasBP,
	(case when itemid in (807,811,1529,3745,3744,225664,220621,226537) and valuenum > 0 then valuenum else null end) as glucose,
	(case when itemid in (456,52,6702,443,220052,220181,225312) then valuenum else null end) as meanArterialPressure
	from mimiciii.chartevents ce
	where ce.error is distinct from 1
	and ce.itemid in (
		223835,
		3420,3422,
		646,220277,
		223761,678,
		223762,676,
		646,220277,
		615,618,220210,224690,
		211,220045,
		51,442,455,6701,220179,220050,
		8368,8440,8441,8555,220180,220051,
 		456,52,6702,443,220052,220181,225312, 
		807,811,1529,3745,3744,225664,220621,226537,
		456,52,6702,443,220052,220181,225312
	)
)
select ce.icustay_id,
	ce.charttime,
	max(SpO2) as SpO2,
	max(FiO2) as FiO2,
	avg(temperature) as temperature,
	avg(respRate) as respRate,
	avg(heartRate) as heartRate,
	avg(sysBP) as sysBP,
	avg(diasBP) as diasBP,
	avg(glucose) as glucose,
	avg(meanArterialPressure) as meanArterialPressure
from ce
group by ce.icustay_id, ce.charttime
order by ce.icustay_id, ce.charttime;

create unique index vitals_icu_id on pv_vitals(icustay_id,charttime);

-- checks etc
select * from pv_vitals pv limit 10;s
