/* Extract feeding data from MIMIC
 * 
 *
 *
 *
220952	Dextrose 50%	Dextrose 50%	metavision	inputevents_mv
225947	Dextrose PN	    Dextrose PN	    metavision	inputevents_mv
228140	Dextrose 20%	Dextrose 20%	metavision	inputevents_mv
228141	Dextrose 30%	Dextrose 30%	metavision	inputevents_mv
228142	Dextrose 40%	Dextrose 40%	metavision	inputevents_mv
220949	Dextrose 5%	    Dextrose 5%	    metavision	inputevents_mv
220950	Dextrose 10%	Dextrose 10%	metavision	inputevents_mv

225823	D5 1/2NS
225825	D5NS
225827	D5LR
225941	D5 1/4NS

227090	Lipids 10%	Lipids 10%	metavision	inputevents_mv	Nutrition - Parenteral	mL	Solution
222190	Ranitidine	Ranitidine	metavision	inputevents_mv	Nutrition - Parenteral	mg	Solution
225916	TPN w/ Lipids	TPN w/ Lipids	metavision	inputevents_mv	Nutrition - Parenteral	mL	Solution
225801	Lipids 20%	Lipids 20%	metavision	inputevents_mv	Nutrition - Parenteral	mL	Solution
225969	Lipids (additive)	Lipids (additive)	metavision	inputevents_mv	Nutrition - Parenteral	grams	Solution
225917	TPN without Lipids	TPN without Lipids	metavision	inputevents_mv	Nutrition - Parenteral	mL	Solution
225920	Peripheral Parenteral Nutrition	Peripheral Parenteral Nutrition	metavision	inputevents_mv	Nutrition - Parenteral	mL	Solution
225947	Dextrose PN	Dextrose PN	metavision	inputevents_mv	Nutrition - Parenteral	grams	Solution
225948	Amino Acids	Amino Acids	metavision	inputevents_mv	Nutrition - Parenteral	grams	Solution
*/

drop materialized view if exists pv_nutrition;
create materialized view pv_nutrition as
with pn_0 as (
	select im.icustay_id,
	hadm_id,
	cast(starttime as timestamp) as starttime,
	cast(endtime as timestamp) as endtime,
	(case 
		when itemid in (220949,225823,225825,225827,225941) and amount >= 0 then amount*0.05
		when itemid in (220950) and amount >= 0 then amount*0.1
		when itemid in (228140) and amount >= 0 then amount*0.2 
		when itemid in (228141) and amount >= 0 then amount*0.3 
		when itemid in (228142) and amount >= 0 then amount*0.4 
		when itemid in (220952) and amount >= 0 then amount*0.5 
	else null end) as dextrose_fluid,
	(case when itemid in (225916,225917) and amount >= 0 then amount 
		else null end) as tpn,
	(case 
		when itemid = 225916 and amount >= 0 then 1
		when itemid = 225917 and amount >= 0 then 0
		else null end) as tpn_lipids,
	(case 
		when itemid in (225947) and amount >= 0 then amount 
		else null end) as dextrose_tpn,
	(case 
		when itemid in (225969) and amount >= 0 then amount 
		else null end) as lipids_tpn,
	(case
		when itemid in (225948) and amount >= 0 then amount 
		else null end) as amino_acids_tpn,
	(case 
		when itemid in (227090) and amount >= 0 then amount*0.1
		when itemid in (225801) and amount >= 0 then amount*0.2
		else null end) as lipids_10_20,
	amountuom as original_amountuom,
	(case 
		when itemid in (220949,220950,228140,228141,228142,220952) 
		and lower(amountuom) = 'ml' 
		then 'grams'
	else amountuom end) as amountuom,
	ordercategoryname ,
	ordercategorydescription
	from mimiciii.inputevents_mv im 
	where itemid in (220949,220950,228140,228141,228142,220952,  --solution dextrose
					 225947,  --parenteral dextrose
					 227090,225801,  --parenteral lipid 10%, 20%
					 225969,  --lipids TPN
					 225948,  --amino acids TPN
					 225916,225917  --total parenteral nutrition
					 ) and 
	statusdescription is distinct from 'Rewritten'
	order by icustay_id, starttime
)
select pn_0.icustay_id,
	pn_0.starttime,
	pn_0.endtime,
	sum(dextrose_fluid) as dextrose_fluid,
	sum(tpn) as tpn,
	max(tpn_lipids) as tpn_lipids,
	sum(dextrose_tpn) as dextrose_tpn,
	sum(lipids_tpn) as lipids_tpn,
	sum(amino_acids_tpn) as amino_acids_tpn,
	sum(lipids_10_20) as lipids_10_20
from pn_0
group by pn_0.icustay_id, pn_0.starttime, pn_0.endtime
order by pn_0.icustay_id, pn_0.starttime, pn_0.endtime;

------ tests -----------
select *
from pv_nutrition
where icustay_id = 207299;

select * from mimic.mimiciii.inputevents_mv where itemid = 225969;

select * from mimic.mimiciii.inputevents_mv
where icustay_id = 200087 and  
	itemid in(225916,225969,225948,225947) and 
	statusdescription is distinct from 'Rewritten'
order by starttime, row_id;

select *
from mimiciii.d_items di 
where di.label like '%Dextrose%' and dbsource = 'metavision' and linksto = 'inputevents_mv';

select *
from mimiciii.d_items di 
where category = 'Fluids/Intake' and dbsource = 'metavision' and linksto = 'inputevents_mv';

select *
from mimiciii.d_items di 
where category = 'Nutrition - Parenteral' and dbsource = 'metavision' and linksto = 'inputevents_mv';

select *  
from mimiciii.d_items di 
where lower(label) like '%heparin%' and dbsource = 'metavision'
limit 100;
-------------------------

/*
221036	Nutren Renal (Full)
221207	Impact (Full)
225928	Impact with Fiber (Full)
225929	ProBalance (Full)
225930	Peptamen 1.5 (Full)
225931	Nutren 2.0 (Full)
225934	Vivonex (Full)
225935	Replete (Full)
225936	Replete with Fiber (Full)
225937	Ensure (Full)
225970	Beneprotein
226016	Nutren 2.0 (1/4)
226017	Nutren 2.0 (2/3)
226018	Nutren 2.0 (3/4)
226019	Nutren 2.0 (1/2)
226020	Impact (1/4)
226021	Impact (2/3)
226022	Impact (3/4)
226023	Impact (1/2)
226024	Impact with Fiber (1/4)
226025	Impact with Fiber (2/3)
226026	Impact with Fiber (3/4)
226027	Impact with Fiber (1/2)
226028	Nutren Renal (1/4)
226029	Nutren Renal (2/3)
226030	Nutren Renal (3/4)
226031	Nutren Renal (1/2)
226036	Peptamen 1.5 (1/4)
226037	Peptamen 1.5 (2/3)
226038	Peptamen 1.5 (3/4)
226039	Peptamen 1.5 (1/2)
226040	ProBalance (1/4)
226041	ProBalance (2/3)
226042	ProBalance (3/4)
226043	ProBalance (1/2)
226044	Replete (1/4)
226045	Replete (2/3)
226046	Replete (3/4)
226047	Replete (1/2)
226048	Replete with Fiber (1/4)
226049	Replete with Fiber (2/3)
226050	Replete with Fiber (3/4)
226051	Replete with Fiber (1/2)
226056	Vivonex (1/4)
226057	Vivonex (2/3)
226058	Vivonex (3/4)
226059	Vivonex (1/2)
226226	Enteral Nutriton Residuals
226875	Ensure (3/4)
226876	Ensure (1/2)
226877	Ensure Plus (Full)
226878	Ensure Plus (3/4)
226879	Ensure Plus (1/2)
226880	Nutren Pulmonary (Full)
226881	Nutren Pulmonary (3/4)
226882	Nutren Pulmonary (1/2)
227091	Ensure (1/4)
227092	Ensure Plus (1/4)
227370	Peptamen VHP (Full)
227371	Peptamen VHP (3/4)
227372	Peptamen VHP (2/3)
227373	Peptamen VHP (1/4)
227374	Peptamen VHP (1/2)
227518	Nutren 2.0 (3/4)
227695	Fibersource HN (Full)
227696	Fibersource HN (3/4)
227697	Fibersource HN (2/3)
227698	Fibersource HN (1/2)
227699	Fibersource HN (1/4)
227972	NovaSource Renal (1/4)
227973	NovaSource Renal (1/2)
227974	NovaSource Renal (3/4)
227975	NovaSource Renal (Full)
227976	Boost Glucose Control (1/4)
227977	Boost Glucose Control (1/2)
227978	Boost Glucose Control (3/4)
227979	Boost Glucose Control (Full)
228131	Isosource 1.5 (1/2)
228132	Isosource 1.5 (1/4)
228133	Isosource 1.5 (2/3)
228134	Isosource 1.5 (3/4)
228135	Isosource 1.5 (Full)
228348	Nepro (1/2)
228349	Nepro (1/4)
228350	Nepro (3/4)
228351	Nepro (Full)
228352	Enlive (1/2)
228353	Enlive (1/4)
228354	Enlive (3/4)
228355	Enlive (Full)
228356	Glucerna (1/2)
228357	Glucerna (1/4)
228358	Glucerna (3/4)
228359	Glucerna (Full)
228360	Pulmocare (1/2)
228361	Pulmocare (1/4)
228362	Pulmocare (3/4)
228363	Pulmocare (Full)
228364	Two Cal HN (1/2)
228365	Two Cal HN (1/4)
228366	Two Cal HN (3/4)
228367	Two Cal HN (Full)
228383	Peptamen Bariatric (Full)*/

drop materialized view if exists ent_nutrition;
create materialized view ent_nutrition as
	select 
	im.icustay_id,
	im.hadm_id,
	im.subject_id,
	cast(im.starttime as timestamp) as starttime,
	cast(im.endtime as timestamp) as endtime,
	(case 
		when itemid in (227977,227976,227978,227979) and amount >= 0 then amount  --Boost Glucose Control 
		when itemid in (228352,228353,228354,228355) and amount >= 0 then amount  --Enlive 
		when itemid in (226876,227091,226875,225937) and amount >= 0 then amount  --Ensure 
		when itemid in (226879,227092,226878,226877) and amount >= 0 then amount  --Ensure Plus 
		when itemid in (227698,227699,227697,227696,227695) and amount >= 0 then amount  --Fibersource HN 
		when itemid in (228356,228357,228358,228359) and amount >= 0 then amount  --Glucerna 
		when itemid in (226023,226020,226021,226022,221207) and amount >= 0 then amount  --Impact 
		when itemid in (226027,226024,226025,226026,225928) and amount >= 0 then amount  --Impact with Fiber  
		when itemid in (228131,228132,228133,228134,228135) and amount >= 0 then amount  --Isosource 1.5  
		when itemid in (228348,228349,228350,228351) and amount >= 0 then amount  --Nepro  
		when itemid in (227973,227972,227974,227975) and amount >= 0 then amount  --NovaSource Renal  
		when itemid in (226019,226016,226017,227518,226018,225931) and amount >= 0 then amount  --Nutren 2.0
		when itemid in (226882,226881,226880) and amount >= 0 then amount  --Nutren Pulmonary
		when itemid in (226031,226028,226029,226030,221036) and amount >= 0 then amount  --Nutren Renal
		when itemid in (226039,226036,226037,226038,225930) and amount >= 0 then amount  --Peptamen 1.5
		when itemid in (228383) and amount >= 0 then amount  --Peptamen Bariatric
		when itemid in (227374,227373,227372,227371,227370) and amount >= 0 then amount  --Peptamen VHP
		when itemid in (226043,226040,226041,226042,225929) and amount >= 0 then amount  --ProBalance
		when itemid in (228360,228361,228362,228363) and amount >= 0 then amount  --Pulmocare
		when itemid in (226047,226044,226045,226046,225935) and amount >= 0 then amount  --Replete
		when itemid in (226051,226048,226049,226050,225936) and amount >= 0 then amount  --Replete with Fiber
		when itemid in (228364,228365,228366,228367) and amount >= 0 then amount  --Two Cal HN 
		when itemid in (226059,226056,226057,226058,225934) and amount >= 0 then amount  --Vivonex 
	else null end) as amount_enteral,
	(case 
		when itemid in (227977,227976,227978,227979) and amount >= 0 then amount*0.067510549  --Boost Glucose Control 
		when itemid in (228352,228353,228354,228355) and amount >= 0 then amount*0.189873418  --Enlive 
		when itemid in (226876,227091,226875,225937) and amount >= 0 then amount*0.139240506  --Ensure 
		when itemid in (226879,227092,226878,226877) and amount >= 0 then amount*0.202531646  --Ensure Plus 
		when itemid in (227698,227699,227697,227696,227695) and amount >= 0 then amount*0.156118143  --Fibersource HN 
		when itemid in (228356,228357,228358,228359) and amount >= 0 then amount*0.11  --Glucerna 
		when itemid in (226023,226020,226021,226022,221207) and amount >= 0 then amount*0.132  --Impact 
		when itemid in (226027,226024,226025,226026,225928) and amount >= 0 then amount*0.132  --Impact with Fiber  
		when itemid in (228131,228132,228133,228134,228135) and amount >= 0 then amount*0.167088608  --Isosource 1.5  
		when itemid in (228348,228349,228350,228351) and amount >= 0 then amount*0.147272727  --Nepro  
		when itemid in (227973,227972,227974,227975) and amount >= 0 then amount*0.184810127  --NovaSource Renal  
		when itemid in (226019,226016,226017,227518,226018,225931) and amount >= 0 then amount*0.196  --Nutren 2.0
		when itemid in (226882,226881,226880) and amount >= 0 then amount*0.1  --Nutren Pulmonary
		when itemid in (226031,226028,226029,226030,221036) and amount >= 0 then amount*0.184810127  --Nutren Renal
		when itemid in (226039,226036,226037,226038,225930) and amount >= 0 then amount*0.188  --Peptamen 1.5
		when itemid in (228383) and amount >= 0 then amount*0.078  --Peptamen Bariatric
		when itemid in (227374,227373,227372,227371,227370) and amount >= 0 then amount*0.076  --Peptamen VHP
		when itemid in (226043,226040,226041,226042,225929) and amount >= 0 then null  --ProBalance
		when itemid in (228360,228361,228362,228363) and amount >= 0 then amount*0.105485232  --Pulmocare
		when itemid in (226047,226044,226045,226046,225935) and amount >= 0 then amount*0.112  --Replete
		when itemid in (226051,226048,226049,226050,225936) and amount >= 0 then amount*0.124  --Replete with Fiber
		when itemid in (228364,228365,228366,228367) and amount >= 0 then amount*0.21  --Two Cal HN 
		when itemid in (226059,226056,226057,226058,225934) and amount >= 0 then amount*0.00256  --Vivonex 
	else null end) as cho_enteral,
	(case 
		when itemid in (227977,227976,227978,227979) and amount >= 0 then amount*0.016877637  --Boost Glucose Control 
		when itemid in (228352,228353,228354,228355) and amount >= 0 then amount*0.092827004 --Enlive 
		when itemid in (226876,227091,226875,225937) and amount >= 0 then amount*0.063291139  --Ensure 
		when itemid in (226879,227092,226878,226877) and amount >= 0 then amount*0.092827004  --Ensure Plus 
		when itemid in (227698,227699,227697,227696,227695) and amount >= 0 then amount*0.010126582  --Fibersource HN 
		when itemid in (228356,228357,228358,228359) and amount >= 0 then amount*0.029  --Glucerna 
		when itemid in (226023,226020,226021,226022,221207) and amount >= 0 then null  --Impact 
		when itemid in (226027,226024,226025,226026,225928) and amount >= 0 then null  --Impact with Fiber  
		when itemid in (228131,228132,228133,228134,228135) and amount >= 0 then amount*0.081012658  --Isosource 1.5  
		when itemid in (228348,228349,228350,228351) and amount >= 0 then amount*0.031818182 --Nepro  
		when itemid in (227973,227972,227974,227975) and amount >= 0 then amount*0.076793249  --NovaSource Renal  
		when itemid in (226019,226016,226017,227518,226018,225931) and amount >= 0 then amount*0.036  --Nutren 2.0
		when itemid in (226882,226881,226880) and amount >= 0 then amount*0.048  --Nutren Pulmonary
		when itemid in (226031,226028,226029,226030,221036) and amount >= 0 then amount*0.076793249  --Nutren Renal
		when itemid in (226039,226036,226037,226038,225930) and amount >= 0 then null  --Peptamen 1.5
		when itemid in (228383) and amount >= 0 then null  --Peptamen Bariatric
		when itemid in (227374,227373,227372,227371,227370) and amount >= 0 then amount*0.0  --Peptamen VHP
		when itemid in (226043,226040,226041,226042,225929) and amount >= 0 then null  --ProBalance
		when itemid in (228360,228361,228362,228363) and amount >= 0 then null  --Pulmocare
		when itemid in (226047,226044,226045,226046,225935) and amount >= 0 then null  --Replete
		when itemid in (226051,226048,226049,226050,225936) and amount >= 0 then amount*0.04  --Replete with Fiber
		when itemid in (228364,228365,228366,228367) and amount >= 0 then amount*0.046  --Two Cal HN 
		when itemid in (226059,226056,226057,226058,225934) and amount >= 0 then amount*0.00256  --Vivonex 
	else null end) as dextrose_enteral,
	(case 
		when itemid in (227977,227976,227978,227979) and amount >= 0 then amount*0.029535865  --Boost Glucose Control 
		when itemid in (228352,228353,228354,228355) and amount >= 0 then amount*0.046413502 --Enlive 
		when itemid in (226876,227091,226875,225937) and amount >= 0 then amount*0.025316456  --Ensure 
		when itemid in (226879,227092,226878,226877) and amount >= 0 then amount*0.046413502  --Ensure Plus 
		when itemid in (227698,227699,227697,227696,227695) and amount >= 0 then amount*0.04092827  --Fibersource HN 
		when itemid in (228356,228357,228358,228359) and amount >= 0 then amount*0.034 --Glucerna 
		when itemid in (226023,226020,226021,226022,221207) and amount >= 0 then amount*0.028  --Impact 
		when itemid in (226027,226024,226025,226026,225928) and amount >= 0 then amount*0.028  --Impact with Fiber  
		when itemid in (228131,228132,228133,228134,228135) and amount >= 0 then amount*0.064135021  --Isosource 1.5  
		when itemid in (228348,228349,228350,228351) and amount >= 0 then amount*0.097727273 --Nepro  
		when itemid in (227973,227972,227974,227975) and amount >= 0 then amount*0.1  --NovaSource Renal  
		when itemid in (226019,226016,226017,227518,226018,225931) and amount >= 0 then amount*0.092  --Nutren 2.0
		when itemid in (226882,226881,226880) and amount >= 0 then amount*0.0948  --Nutren Pulmonary
		when itemid in (226031,226028,226029,226030,221036) and amount >= 0 then amount*0.1  --Nutren Renal
		when itemid in (226039,226036,226037,226038,225930) and amount >= 0 then amount*0.056 --Peptamen 1.5
		when itemid in (228383) and amount >= 0 then amount*0.038  --Peptamen Bariatric
		when itemid in (227374,227373,227372,227371,227370) and amount >= 0 then amount*0.038  --Peptamen VHP
		when itemid in (226043,226040,226041,226042,225929) and amount >= 0 then null  --ProBalance
		when itemid in (228360,228361,228362,228363) and amount >= 0 then amount*0.093248945  --Pulmocare
		when itemid in (226047,226044,226045,226046,225935) and amount >= 0 then amount*0.034  --Replete
		when itemid in (226051,226048,226049,226050,225936) and amount >= 0 then amount*0.034  --Replete with Fiber
		when itemid in (228364,228365,228366,228367) and amount >= 0 then amount*0.089  --Two Cal HN 
		when itemid in (226059,226056,226057,226058,225934) and amount >= 0 then amount*0.004  --Vivonex 
	else null end) as fat_enteral,
	(case 
		when itemid in (227977,227976,227978,227979) and amount >= 0 then amount*0.067510549  --Boost Glucose Control 
		when itemid in (228352,228353,228354,228355) and amount >= 0 then amount*0.084388186 --Enlive 
		when itemid in (226876,227091,226875,225937) and amount >= 0 then amount*0.037974684  --Ensure 
		when itemid in (226879,227092,226878,226877) and amount >= 0 then amount*0.067510549  --Ensure Plus 
		when itemid in (227698,227699,227697,227696,227695) and amount >= 0 then amount*0.054008439 --Fibersource HN 
		when itemid in (228356,228357,228358,228359) and amount >= 0 then amount*0.0465 --Glucerna 
		when itemid in (226023,226020,226021,226022,221207) and amount >= 0 then amount*0.056  --Impact 
		when itemid in (226027,226024,226025,226026,225928) and amount >= 0 then amount*0.056 --Impact with Fiber  
		when itemid in (228131,228132,228133,228134,228135) and amount >= 0 then amount*0.064978903  --Isosource 1.5  
		when itemid in (228348,228349,228350,228351) and amount >= 0 then amount*0.080909091 --Nepro  
		when itemid in (227973,227972,227974,227975) and amount >= 0 then amount*0.091139241  --NovaSource Renal  
		when itemid in (226019,226016,226017,227518,226018,225931) and amount >= 0 then amount*0.08  --Nutren 2.0
		when itemid in (226882,226881,226880) and amount >= 0 then amount*0.068  --Nutren Pulmonary
		when itemid in (226031,226028,226029,226030,221036) and amount >= 0 then amount*0.091139241  --Nutren Renal
		when itemid in (226039,226036,226037,226038,225930) and amount >= 0 then amount*0.068 --Peptamen 1.5
		when itemid in (228383) and amount >= 0 then amount*0.0932  --Peptamen Bariatric
		when itemid in (227374,227373,227372,227371,227370) and amount >= 0 then amount*0.092  --Peptamen VHP
		when itemid in (226043,226040,226041,226042,225929) and amount >= 0 then null  --ProBalance
		when itemid in (228360,228361,228362,228363) and amount >= 0 then amount*0.062447257  --Pulmocare
		when itemid in (226047,226044,226045,226046,225935) and amount >= 0 then amount*0.064  --Replete
		when itemid in (226051,226048,226049,226050,225936) and amount >= 0 then amount*0.064  --Replete with Fiber
		when itemid in (228364,228365,228366,228367) and amount >= 0 then amount*0.084  --Two Cal HN 
		when itemid in (226059,226056,226057,226058,225934) and amount >= 0 then amount*0.044  --Vivonex 
	else null end) as protein_enteral,
	(case 
		when itemid in (227977,227976,227978,227979) and amount >= 0 then null  --Boost Glucose Control 
		when itemid in (228352,228353,228354,228355) and amount >= 0 then null --Enlive 
		when itemid in (226876,227091,226875,225937) and amount >= 0 then null  --Ensure 
		when itemid in (226879,227092,226878,226877) and amount >= 0 then null  --Ensure Plus 
		when itemid in (227698,227699,227697,227696,227695) and amount >= 0 then null --Fibersource HN 
		when itemid in (228356,228357,228358,228359) and amount >= 0 then null --Glucerna 
		when itemid in (226023,226020,226021,226022,221207) and amount >= 0 then null  --Impact 
		when itemid in (226027,226024,226025,226026,225928) and amount >= 0 then null --Impact with Fiber  
		when itemid in (228131,228132,228133,228134,228135) and amount >= 0 then null  --Isosource 1.5  
		when itemid in (228348,228349,228350,228351) and amount >= 0 then null --Nepro  
		when itemid in (227973,227972,227974,227975) and amount >= 0 then null  --NovaSource Renal  
		when itemid in (226019,226016,226017,227518,226018,225931) and amount >= 0 then null  --Nutren 2.0
		when itemid in (226882,226881,226880) and amount >= 0 then null  --Nutren Pulmonary
		when itemid in (226031,226028,226029,226030,221036) and amount >= 0 then null  --Nutren Renal
		when itemid in (226039,226036,226037,226038,225930) and amount >= 0 then null --Peptamen 1.5
		when itemid in (228383) and amount >= 0 then null  --Peptamen Bariatric
		when itemid in (227374,227373,227372,227371,227370) and amount >= 0 then null  --Peptamen VHP
		when itemid in (226043,226040,226041,226042,225929) and amount >= 0 then null  --ProBalance
		when itemid in (228360,228361,228362,228363) and amount >= 0 then null  --Pulmocare
		when itemid in (226047,226044,226045,226046,225935) and amount >= 0 then null  --Replete
		when itemid in (226051,226048,226049,226050,225936) and amount >= 0 then null  --Replete with Fiber
		when itemid in (228364,228365,228366,228367) and amount >= 0 then null  --Two Cal HN 
		when itemid in (226059,226056,226057,226058,225934) and amount >= 0 then 0  --Vivonex 
	else null end) as fibre_enteral,
	(case
		when itemid in (227977,227976,227978,227979) and amount >= 0 then amount*0.801687764  --Boost Glucose Control 
		when itemid in (228352,228353,228354,228355) and amount >= 0 then amount*1.476793249 --Enlive 
		when itemid in (226876,227091,226875,225937) and amount >= 0 then amount*0.928270042  --Ensure 
		when itemid in (226879,227092,226878,226877) and amount >= 0 then amount*1.476793249  --Ensure Plus 
		when itemid in (227698,227699,227697,227696,227695) and amount >= 0 then amount*1.223628692 --Fibersource HN 
		when itemid in (228356,228357,228358,228359) and amount >= 0 then amount*0.93 --Glucerna 
		when itemid in (226023,226020,226021,226022,221207) and amount >= 0 then amount*1.0  --Impact 
		when itemid in (226027,226024,226025,226026,225928) and amount >= 0 then amount*1.0 --Impact with Fiber  
		when itemid in (228131,228132,228133,228134,228135) and amount >= 0 then amount*1.518987342  --Isosource 1.5  
		when itemid in (228348,228349,228350,228351) and amount >= 0 then amount*1.822727273 --Nepro  
		when itemid in (227973,227972,227974,227975) and amount >= 0 then amount*2.0  --NovaSource Renal  
		when itemid in (226019,226016,226017,227518,226018,225931) and amount >= 0 then amount*2.0  --Nutren 2.0
		when itemid in (226882,226881,226880) and amount >= 0 then amount*1.5  --Nutren Pulmonary
		when itemid in (226031,226028,226029,226030,221036) and amount >= 0 then amount*2.004219409  --Nutren Renal
		when itemid in (226039,226036,226037,226038,225930) and amount >= 0 then amount*1.5 --Peptamen 1.5
		when itemid in (228383) and amount >= 0 then amount*1.0  --Peptamen Bariatric
		when itemid in (227374,227373,227372,227371,227370) and amount >= 0 then amount*1.0  --Peptamen VHP
		when itemid in (226043,226040,226041,226042,225929) and amount >= 0 then null  --ProBalance
		when itemid in (228360,228361,228362,228363) and amount >= 0 then amount*1.497890295  --Pulmocare
		when itemid in (226047,226044,226045,226046,225935) and amount >= 0 then amount*1.0  --Replete
		when itemid in (226051,226048,226049,226050,225936) and amount >= 0 then amount*1.0  --Replete with Fiber
		when itemid in (228364,228365,228366,228367) and amount >= 0 then amount*1.995  --Two Cal HN 
		when itemid in (226059,226056,226057,226058,225934) and amount >= 0 then amount*1.212  --Vivonex 
	else null end) as calorie_enteral
	from mimiciii.inputevents_mv im 
	where itemid in (
			 227977,227976,227978,227979,   --Boost Glucose Control 
			228352,228353,228354,228355, --Enlive 
			226876,227091,226875,225937,   --Ensure 
			226879,227092,226878,226877,  --Ensure Plus 
			227698,227699,227697,227696,227695,  --Fibersource HN 
			228356,228357,228358,228359,  --Glucerna 
			226023,226020,226021,226022,221207, --Impact 
			226027,226024,226025,226026,225928,  --Impact with Fiber  
			228131,228132,228133,228134,228135, --Isosource 1.5  
			228348,228349,228350,228351,  --Nepro  
			227973,227972,227974,227975,   --NovaSource Renal  
			226019,226016,226017,227518,226018,225931,  --Nutren 2.0
			226882,226881,226880,  --Nutren Pulmonary
			226031,226028,226029,226030,221036,   --Nutren Renal
			226039,226036,226037,226038,225930,  --Peptamen 1.5
			228383,   --Peptamen Bariatric
			227374,227373,227372,227371,227370,   --Peptamen VHP
			226043,226040,226041,226042,225929,  --ProBalance
			228360,228361,228362,228363,   --Pulmocare
			226047,226044,226045,226046,225935,   --Replete
			226051,226048,226049,226050,225936,  --Replete with Fiber
			228364,228365,228366,228367,   --Two Cal HN 
			226059,226056,226057,226058,225934   --Vivonex 
		) and statusdescription is distinct from 'Rewritten'
	order by icustay_id, starttime;

--- test
select * from ent_nutrition;

select * 
from ent_nutrition 
where icustay_id = 205390
order by icustay_id, starttime;


