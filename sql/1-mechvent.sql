/* Mechanical ventilation time duration and parameters

TODO:
-- check applicability of summary measure (i.e. avg(tidalvolume) as peep etc.)
*/

drop materialized view if exists pv_mechvent cascade;
create materialized view pv_mechvent as

with stg as (
	select c.icustay_id, c.charttime,
			avg(case when itemid in (445,448,449,450,1340,1486,1600,224687) 
				then valuenum else null end) as minuteVolume,
			avg(case when itemid in (683,224684)
				then valuenum else null end) as setTidalVolume,
			avg(case when itemid in (681,682,224685)
				then valuenum else null end) as obsTidalVolume,
			avg(case when itemid in (654,684,224686)
				then valuenum else null end) as spontTidalVolume,
			avg(case when itemid in (505,506,220339)
				then valuenum else null end) as setPEEP,
			avg(case when itemid in (686,224700)
				then valuenum else null end) as totalPEEP,
			avg(case when itemid in (5865,224705)
				then valuenum else null end) as pressureHighAPRV,
			avg(case when itemid in (5866,224706)
				then valuenum else null end) as pressureLowAPRV,
			avg(case when itemid in (224707)
				then valuenum else null end) as timeHighAPRV,
			avg(case when itemid in (224709)
				then valuenum else null end) as timeLowAPRV,
			avg(case when itemid in (444,224697)
				then valuenum else null end) as meanAirwayPressure,
			avg(case when itemid in (535,224695)
				then valuenum else null end) as peakInspPressure,
			avg(case 
				when itemid in (459) then -valuenum
				when itemid in (224419) then valuenum
				else null
			end) as negInspForce ,
			avg(case when itemid in (1655,224738) 
				then valuenum else null end) as inspTime,
			avg(case when itemid in (543,224696) 
				then valuenum else null end) as plateauPressure
	from mimiciii.chartevents c
	where c.itemid in (
			445, 448, 449, 450, 1340, 1486, 1600, 224687,  -- minute volume
			639, 654, 681, 682, 683, 684,224685,224684,224686, -- tidal volume
			5865,5866,224707,224709,224705,224706, --APRV pressure and time
			505,506,686,220339,224700, --PEEP
			444,224697, -- mean airway pressure
			535,224695, -- peak inspiratory pressure
			459,224419, -- negative inspiratory force
			1655,224738, -- inspiratory time
			543,224696 -- plateau pressure
			)
	and c.error is distinct from 1
	group by c.icustay_id,c.charttime
	order by c.icustay_id,c.charttime
)

select v.icustay_id,
		s.charttime,
		v.starttime,
		v.endtime,
		v.duration_hours,
		v.ventnum,
		minuteVolume,
		setTidalVolume,
		obsTidalVolume,
		spontTidalVolume,
		setPEEP,
		totalPEEP,
		pressureHighAPRV,
		pressureLowAPRV,
		timeHighAPRV,
		timeLowAPRV,
		meanAirwayPressure,
		peakInspPressure,
		negInspForce ,
		inspTime,
		plateauPressure
from ventdurations v left join stg s on
	v.icustay_id = s.icustay_id and s.charttime >= v.starttime and
	 s.charttime <= v.endtime
order by v.icustay_id,s.charttime;

create unique index ventparam_icu_id on pv_mechvent (icustay_id,charttime);

----- checks etc ---------------


----- end -------------------------------------------------------------------

