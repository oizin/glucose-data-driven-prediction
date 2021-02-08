/* Antibiotics 
totalamount: volume of fluid in which IV medicine was found (metavision)
totalamount: volume of medicine (carevue)
statusdescription: if rewritten then the medicine is over

Metavision order categories:
01-Drips
02-Fluids (Crystalloids)
03-IV Fluid Bolus
04-Fluids (Colloids)
05-Med Bolus
06-Insulin (Non IV)
07-Blood Products
08-Antibiotics (IV)
09-Antibiotics (Non IV)
10-Prophylaxis (IV)
11-Prophylaxis (Non IV)
12-Parenteral Nutrition
13-Enteral Nutrition
14-Oral/Gastric Intake
15-Supplements
16-Pre Admission
 */

drop materialized view if exists antibiotics cascade;
create materialized view antibiotics as

with di as
(
  select
    di.*,
    case
      when lower(label) like '%' || lower('adoxa') || '%' then 1
      when lower(label) like '%' || lower('ala-tet') || '%' then 1
      when lower(label) like '%' || lower('alodox') || '%' then 1
      when lower(label) like '%' || lower('amikacin') || '%' then 1
      when lower(label) like '%' || lower('amikin') || '%' then 1
      when lower(label) like '%' || lower('amoxicillin') || '%' then 1
      when lower(label) like '%' || lower('amoxicillin%clavulanate') || '%' then 1
      when lower(label) like '%' || lower('clavulanate') || '%' then 1
      when lower(label) like '%' || lower('ampicillin') || '%' then 1
      when lower(label) like '%' || lower('augmentin') || '%' then 1
      when lower(label) like '%' || lower('avelox') || '%' then 1
      when lower(label) like '%' || lower('avidoxy') || '%' then 1
      when lower(label) like '%' || lower('azactam') || '%' then 1
      when lower(label) like '%' || lower('azithromycin') || '%' then 1
      when lower(label) like '%' || lower('aztreonam') || '%' then 1
      when lower(label) like '%' || lower('axetil') || '%' then 1
      when lower(label) like '%' || lower('bactocill') || '%' then 1
      when lower(label) like '%' || lower('bactrim') || '%' then 1
      when lower(label) like '%' || lower('bethkis') || '%' then 1
      when lower(label) like '%' || lower('biaxin') || '%' then 1
      when lower(label) like '%' || lower('bicillin l-a') || '%' then 1
      when lower(label) like '%' || lower('cayston') || '%' then 1
      when lower(label) like '%' || lower('cefazolin') || '%' then 1
      when lower(label) like '%' || lower('cedax') || '%' then 1
      when lower(label) like '%' || lower('cefoxitin') || '%' then 1
      when lower(label) like '%' || lower('ceftazidime') || '%' then 1
      when lower(label) like '%' || lower('cefaclor') || '%' then 1
      when lower(label) like '%' || lower('cefadroxil') || '%' then 1
      when lower(label) like '%' || lower('cefdinir') || '%' then 1
      when lower(label) like '%' || lower('cefditoren') || '%' then 1
      when lower(label) like '%' || lower('cefepime') || '%' then 1
      when lower(label) like '%' || lower('cefotetan') || '%' then 1
      when lower(label) like '%' || lower('cefotaxime') || '%' then 1
      when lower(label) like '%' || lower('cefpodoxime') || '%' then 1
      when lower(label) like '%' || lower('cefprozil') || '%' then 1
      when lower(label) like '%' || lower('ceftibuten') || '%' then 1
      when lower(label) like '%' || lower('ceftin') || '%' then 1
      when lower(label) like '%' || lower('cefuroxime ') || '%' then 1
      when lower(label) like '%' || lower('cefuroxime') || '%' then 1
      when lower(label) like '%' || lower('cephalexin') || '%' then 1
      when lower(label) like '%' || lower('chloramphenicol') || '%' then 1
      when lower(label) like '%' || lower('cipro') || '%' then 1
      when lower(label) like '%' || lower('ciprofloxacin') || '%' then 1
      when lower(label) like '%' || lower('claforan') || '%' then 1
      when lower(label) like '%' || lower('clarithromycin') || '%' then 1
      when lower(label) like '%' || lower('cleocin') || '%' then 1
      when lower(label) like '%' || lower('clindamycin') || '%' then 1
      when lower(label) like '%' || lower('cubicin') || '%' then 1
      when lower(label) like '%' || lower('dicloxacillin') || '%' then 1
      when lower(label) like '%' || lower('doryx') || '%' then 1
      when lower(label) like '%' || lower('doxycycline') || '%' then 1
      when lower(label) like '%' || lower('duricef') || '%' then 1
      when lower(label) like '%' || lower('dynacin') || '%' then 1
      when lower(label) like '%' || lower('ery-tab') || '%' then 1
      when lower(label) like '%' || lower('eryped') || '%' then 1
      when lower(label) like '%' || lower('eryc') || '%' then 1
      when lower(label) like '%' || lower('erythrocin') || '%' then 1
      when lower(label) like '%' || lower('erythromycin') || '%' then 1
      when lower(label) like '%' || lower('factive') || '%' then 1
      when lower(label) like '%' || lower('flagyl') || '%' then 1
      when lower(label) like '%' || lower('fortaz') || '%' then 1
      when lower(label) like '%' || lower('furadantin') || '%' then 1
      when lower(label) like '%' || lower('garamycin') || '%' then 1
      when lower(label) like '%' || lower('gentamicin') || '%' then 1
      when lower(label) like '%' || lower('kanamycin') || '%' then 1
      when lower(label) like '%' || lower('keflex') || '%' then 1
      when lower(label) like '%' || lower('ketek') || '%' then 1
      when lower(label) like '%' || lower('levaquin') || '%' then 1
      when lower(label) like '%' || lower('levofloxacin') || '%' then 1
      when lower(label) like '%' || lower('lincocin') || '%' then 1
      when lower(label) like '%' || lower('macrobid') || '%' then 1
      when lower(label) like '%' || lower('macrodantin') || '%' then 1
      when lower(label) like '%' || lower('maxipime') || '%' then 1
      when lower(label) like '%' || lower('mefoxin') || '%' then 1
      when lower(label) like '%' || lower('metronidazole') || '%' then 1
      when lower(label) like '%' || lower('minocin') || '%' then 1
      when lower(label) like '%' || lower('minocycline') || '%' then 1
      when lower(label) like '%' || lower('monodox') || '%' then 1
      when lower(label) like '%' || lower('monurol') || '%' then 1
      when lower(label) like '%' || lower('morgidox') || '%' then 1
      when lower(label) like '%' || lower('moxatag') || '%' then 1
      when lower(label) like '%' || lower('moxifloxacin') || '%' then 1
      when lower(label) like '%' || lower('myrac') || '%' then 1
      when lower(label) like '%' || lower('nafcillin sodium') || '%' then 1
      when lower(label) like '%' || lower('nicazel doxy 30') || '%' then 1
      when lower(label) like '%' || lower('nitrofurantoin') || '%' then 1
      when lower(label) like '%' || lower('noroxin') || '%' then 1
      when lower(label) like '%' || lower('ocudox') || '%' then 1
      when lower(label) like '%' || lower('ofloxacin') || '%' then 1
      when lower(label) like '%' || lower('omnicef') || '%' then 1
      when lower(label) like '%' || lower('oracea') || '%' then 1
      when lower(label) like '%' || lower('oraxyl') || '%' then 1
      when lower(label) like '%' || lower('oxacillin') || '%' then 1
      when lower(label) like '%' || lower('pc pen vk') || '%' then 1
      when lower(label) like '%' || lower('pce dispertab') || '%' then 1
      when lower(label) like '%' || lower('panixine') || '%' then 1
      when lower(label) like '%' || lower('pediazole') || '%' then 1
      when lower(label) like '%' || lower('penicillin') || '%' then 1
      when lower(label) like '%' || lower('periostat') || '%' then 1
      when lower(label) like '%' || lower('pfizerpen') || '%' then 1
      when lower(label) like '%' || lower('piperacillin') || '%' then 1
      when lower(label) like '%' || lower('tazobactam') || '%' then 1
      when lower(label) like '%' || lower('primsol') || '%' then 1
      when lower(label) like '%' || lower('proquin') || '%' then 1
      when lower(label) like '%' || lower('raniclor') || '%' then 1
      when lower(label) like '%' || lower('rifadin') || '%' then 1
      when lower(label) like '%' || lower('rifampin') || '%' then 1
      when lower(label) like '%' || lower('rocephin') || '%' then 1
      when lower(label) like '%' || lower('smz-tmp') || '%' then 1
      when lower(label) like '%' || lower('septra') || '%' then 1
      when lower(label) like '%' || lower('septra ds') || '%' then 1
      when lower(label) like '%' || lower('septra') || '%' then 1
      when lower(label) like '%' || lower('solodyn') || '%' then 1
      when lower(label) like '%' || lower('spectracef') || '%' then 1
      when lower(label) like '%' || lower('streptomycin sulfate') || '%' then 1
      when lower(label) like '%' || lower('sulfadiazine') || '%' then 1
      when lower(label) like '%' || lower('sulfamethoxazole') || '%' then 1
      when lower(label) like '%' || lower('trimethoprim') || '%' then 1
      when lower(label) like '%' || lower('sulfatrim') || '%' then 1
      when lower(label) like '%' || lower('sulfisoxazole') || '%' then 1
      when lower(label) like '%' || lower('suprax') || '%' then 1
      when lower(label) like '%' || lower('synercid') || '%' then 1
      when lower(label) like '%' || lower('tazicef') || '%' then 1
      when lower(label) like '%' || lower('tetracycline') || '%' then 1
      when lower(label) like '%' || lower('timentin') || '%' then 1
      when lower(label) like '%' || lower('tobi') || '%' then 1
      when lower(label) like '%' || lower('tobramycin') || '%' then 1
      when lower(label) like '%' || lower('trimethoprim') || '%' then 1
      when lower(label) like '%' || lower('unasyn') || '%' then 1
      when lower(label) like '%' || lower('vancocin') || '%' then 1
      when lower(label) like '%' || lower('vancomycin') || '%' then 1
      when lower(label) like '%' || lower('vantin') || '%' then 1
      when lower(label) like '%' || lower('vibativ') || '%' then 1
      when lower(label) like '%' || lower('vibra-tabs') || '%' then 1
      when lower(label) like '%' || lower('vibramycin') || '%' then 1
      when lower(label) like '%' || lower('zinacef') || '%' then 1
      when lower(label) like '%' || lower('zithromax') || '%' then 1
      when lower(label) like '%' || lower('zmax') || '%' then 1
      when lower(label) like '%' || lower('zosyn') || '%' then 1
      when lower(label) like '%' || lower('zyvox') || '%' then 1
    else 0
    end as antibiotic
  from mimiciii.d_items di
  where linksto in ('inputevents_mv','inputevents_cv')
),
di_mv as (
	select icustay_id,
		starttime,
		endtime,
		im.amount as amount,
		im.amountuom as amountuom,
		im.rate,
		im.rateuom,
		im.ordercategoryname,
		im.patientweight,
		im.totalamount,
		im.totalamountuom,
		im.statusdescription,
		di.label,
		di.abbreviation,
		di.antibiotic,
		dbsource
	from mimiciii.inputevents_mv im inner join di on 
		im.itemid = di.itemid
	where di.antibiotic = 1
),
di_cv as (
	select icustay_id,
	(case when amount is null then charttime else null end) as starttime,
	(case when amount is not null then charttime else null end) as endtime,
	amount as amount, 
	amountuom as amountuom,
	rate,
	rateuom,
	null :: text as ordercategoryname,
	null :: real  as patientweight,
	null :: real  as totalamount,
	null :: text as totalamountuom,
	null :: text as statusdescription,
	label,
	--stopped,
	null as	abbreviation,
	di.antibiotic,
	dbsource
	from mimiciii.inputevents_cv ic inner join di on 
		ic.itemid = di.itemid
	where di.antibiotic = 1
)
select * from di_mv 
union 
select * from di_cv
order by icustay_id,starttime

create index antibiotics_icu_id on antibiotics (icustay_id,starttime,endtime);

----- checks etc ---------------
-- select all of the data
select * from antibiotics

-- join hour and day of stay information to the data






















