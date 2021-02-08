
drop materialized view if exists icd9_diag cascade;

create materialized view icd9_diag as

select di.row_id,
		subject_id,
		hadm_id,
		seq_num,
		di.icd9_code,
		did.short_title,
		did.long_title 
from mimiciii.diagnoses_icd di 
left join mimiciii.d_icd_diagnoses did
on di.icd9_code = did.icd9_code 
order by subject_id,seq_num;