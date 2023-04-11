### diagnosis -> ED sepsis ICD coding
# 14 ICD codes
select distinct icd_code, icd_version, icd_title from diagnosis where icd_title like '%sepsis%' or icd_title like '%septic shock%'; 



### diagnosis -> ED sepsis stay ID
# 2407 unique stays
select distinct stay_id from diagnosis where icd_title like '%sepsis%' or icd_title like '%septic shock%'; 

# 2851 diagnosis (pseudo-priority)
select stay_id from diagnosis where icd_title like '%sepsis%' or icd_title like '%septic shock%'; 



### edstays -> stay ID detail (excluding patients discharged from ED directly)
# 2330 stays admitted to hosp
select subject_id, hadm_id, stay_id, intime, outtime, gender from edstays 
where stay_id in (select distinct stay_id from diagnosis where icd_title like '%sepsis%' or icd_title like '%septic shock%')
	and disposition = 'admitted'; 
    
# 1 stay has subject_id, but no hadm_id
select subject_id, hadm_id, stay_id, intime, outtime, gender from edstays 
where stay_id in (select distinct stay_id from diagnosis where icd_title like '%sepsis%' or icd_title like '%septic shock%')
	and disposition = 'admitted' and hadm_id is null;
# no records found for this subject in admissions, need to exclude this patient
select * from admissions where subject_id = 17994554;
   
# 19 stays expired, 22 stays others, 32 stays home, 4 stays transfer
select count(stay_id), disposition from edstays
where stay_id in (select distinct stay_id from diagnosis where icd_title like '%sepsis%' or icd_title like '%septic shock%')
group by disposition; # 19 stays expired (death?), 22 stays others, 32 stays home, 4 stays transfer

# final cohort size: 2329 stays (< 2329 unique patients) (csv)
select subject_id, hadm_id, stay_id, intime, outtime, gender, race from edstays 
where stay_id in (select distinct stay_id from diagnosis where icd_title like '%sepsis%' or icd_title like '%septic shock%')
	and disposition = 'admitted' and hadm_id is not null;

# construct table for later queries
create table edsepsis 
select subject_id, hadm_id, stay_id, intime, outtime, gender, race from edstays 
where stay_id in (select distinct stay_id from diagnosis where icd_title like '%sepsis%' or icd_title like '%septic shock%')
	and disposition = 'admitted' and hadm_id is not null;



################
# Demographics #   
################
### admission -> admit time, death time, hospital_expire_flag (1 death, 0 otherwise) (csv)
select edsepsis.*, admittime, deathtime, hospital_expire_flag
from edsepsis
inner join admissions
on edsepsis.hadm_id = admissions.hadm_id;

# creating two temporaray tables for edsepsis_admission (mysql does not allow more than one use of the same temp table in one query)
create temporary table edsepsis_admission 
select edsepsis.*, admittime, dischtime, deathtime, hospital_expire_flag
from edsepsis
inner join admissions
on edsepsis.hadm_id = admissions.hadm_id;
create temporary table edsepsis_admission_dup
select edsepsis.*, admittime, dischtime, deathtime, hospital_expire_flag
from edsepsis
inner join admissions
on edsepsis.hadm_id = admissions.hadm_id;



### OMR -> BMI (kg/m^2) and weight (lbs, for vasopressor later)
# omr's subject information not unique
select count(distinct subject_id) from omr;
select count(subject_id) from omr;



### patients -> age
# using anchor_age (no missing value) instead of dod (lots of null)
# age = admission time - anchor_year + anchor_age
select edsepsis_admission.*, anchor_age + timestampdiff(year,timestamp(concat(anchor_year, '-01-01')), admittime) age
from edsepsis_admission
left join 
(	
	select subject_id, anchor_age, anchor_year
    from patients
) patients
on edsepsis_admission.subject_id = patients.subject_id;



### admission & OMR & patients (csv)
select edsepsis_admission_dup.*, BMI, weight, anchor_age + timestampdiff(year,timestamp(concat(anchor_year, '-01-01')), admittime) age
from edsepsis_admission_dup
left join # OMR
(
	# not including chartdate, o.w. multiple rows for same pt with diff chartdate for BMI and weight
	select subject_id, 
	max(case when result_name = 'BMI (kg/m2)' then result_value end) BMI,
	max(case when result_name = 'Weight (Lbs)' then result_value end) weight
	from 
    (
		# OMR info entered after ED admit, possibly after discharge from hosp (imputation)
		select edsepsis_admission.subject_id, chartdate, result_name, result_value,
        row_number() over(partition by subject_id, result_name order by chartdate) row_num 
		from 
        (	select subject_id, intime
			from edsepsis_admission
		) edsepsis_admission
		inner join 
        (
			select subject_id, chartdate, result_name, result_value
			from omr 
			where result_name = 'BMI (kg/m2)' or result_name = 'Weight (Lbs)'
		) omr
		on edsepsis_admission.subject_id = omr.subject_id and date(edsepsis_admission.intime) <= omr.chartdate
	) BMI_weight
	where row_num = 1 # first entry after ED admit
	group by subject_id
) BMI_weight_first
on edsepsis_admission_dup.subject_id = BMI_weight_first.subject_id
left join # patients
(	
	select subject_id, anchor_age, anchor_year
    from patients
) patients
on edsepsis_admission_dup.subject_id = patients.subject_id;



###################################
# Pre-existing medical conditions #   
###################################
### d_icd_diagnoses -> ICD codes (need further examination)
# create table by icd versions of diagnosis
create table d_icd_diagnoses_v
select case when icd_version = 9 then icd_code else null end as icd9_code, 
case when icd_version = 10 then icd_code else null end as icd10_code
from d_icd_diagnoses;

# myocardial infarction: 55 codes
select case when icd9_code is null then icd10_code else icd9_code end as MI
from d_icd_diagnoses_v
where substr(icd9_code, 1, 3) in ('410', '412')
or substr(icd10_code, 1, 3) in ('I21', 'I22')
or substr(icd10_code, 1, 4) = 'I252';

# congestive heart failure: 72 codes
select case when icd9_code is null then icd10_code else icd9_code end as CHF
from d_icd_diagnoses_v
where substr(icd9_code, 1, 3) = '428'
or substr(icd9_code, 1, 5) in ('39891', '40201', '40211', '40291', '40401', '40403', '40411', '40413', '40491', '40493')
or substr(icd9_code, 1, 4) between '4254' and '4259'
or substr(icd10_code, 1, 3) in ('I43', 'I50')
or substr(icd10_code, 1, 4) in ('I099', 'I110', 'I130', 'I132', 'I255', 'I420', 'I425', 'I426', 'I427', 'I428', 'I429', 'P290');

# peripheral vascular disease: 372 codes
select case when icd9_code is null then icd10_code else icd9_code end as PVD
from d_icd_diagnoses_v
where substr(icd9_code, 1, 3) in ('440', '441')
or substr(icd9_code, 1, 4) in ('0930', '4373', '4471', '5571', '5579', 'V434')
or substr(icd9_code, 1, 4) between '4431' and '4439'
or substr(icd10_code, 1, 3) in ('I70', 'I71')
or substr(icd10_code, 1, 4) in ('I731', 'I738', 'I739', 'I771', 'I790', 'I792', 'K551', 'K558', 'K559', 'Z958', 'Z959');

# cerebrovascular disease: 601 codes
select case when icd9_code is null then icd10_code else icd9_code end as CD
from d_icd_diagnoses_v
where substr(icd9_code, 1, 3) between '430' and '438'
or substr(icd9_code, 1, 5) = '36234'
or substr(icd10_code, 1, 3) in ('G45', 'G46')
or substr(icd10_code, 1, 3) between 'I60' and 'I69'
or substr(icd10_code, 1, 4) = 'H340';

# dementia: 35 codes
select case when icd9_code is null then icd10_code else icd9_code end as dementia
from d_icd_diagnoses_v
where substr(icd9_code, 1, 3) = '290'
or substr(icd9_code, 1, 4) in ('2941', '3312')
or substr(icd10_code, 1, 3) in ('F00', 'F01', 'F02', 'F03', 'G30')
or substr(icd10_code, 1, 4) in ('F051', 'G311');

# chronic pulmonary disease: 134 codes
select case when icd9_code is null then icd10_code else icd9_code end as CPD
from d_icd_diagnoses_v
where substr(icd9_code, 1, 3) between '490' and '505'
or substr(icd9_code, 1, 4) in ('4168', '4169', '5064', '5081', '5088')
or substr(icd10_code, 1, 3) between 'J40' and 'J47'
or substr(icd10_code, 1, 3) between 'J60' and 'J67'
or substr(icd10_code, 1, 4) in ('I278', 'I279', 'J684', 'J701', 'J703');

# rheumatic disease: 16 codes
select case when icd9_code is null then icd10_code else icd9_code end as rheumatic
from d_icd_diagnoses_v
where substr(icd9_code, 1, 3) = '725'
or substr(icd9_code, 1, 4) in ('4465', '7100', '7101', '7102', '7103', '7104', '7140', '7141', '7142', '7148')
or substr(icd10_code, 1, 4) in ('M315', 'M351', 'M353', 'M360');

# peptic ulcer disease: 112 codes
select case when icd9_code is null then icd10_code else icd9_code end as PUD
from d_icd_diagnoses_v
where substr(icd9_code, 1, 3) in ('531', '532', '533', '534')
or substr(icd10_code, 1, 3) in ('K25', 'K26', 'K27', 'K28');

# mild liver disease: 72 codes
select case when icd9_code is null then icd10_code else icd9_code end as MLD
from d_icd_diagnoses_v
where substr(icd9_code, 1, 3) in ('570', '571')
or substr(icd9_code, 1, 4) in ('0706', '0709', '5733', '5734', '5738', '5739', 'V427')
or substr(icd9_code, 1, 5) in ('07022', '07023', '07032', '07033', '07044', '07054')
or substr(icd10_code, 1, 3) in ('B18', 'K73', 'K74')
or substr(icd10_code, 1, 4) in ('K700', 'K701', 'K702', 'K703', 'K709', 'K713', 'K714', 'K715', 'K717', 
								'K760', 'K762', 'K763', 'K764', 'K768', 'K769', 'Z944');

# diabetes without chronic complication: 93 rows
select case when icd9_code is null then icd10_code else icd9_code end as DCC
from d_icd_diagnoses_v
where substr(icd9_code, 1, 4) in ('2500', '2501', '2502', '2503', '2508', '2509')
or substr(icd10_code, 1, 4) in ('E100', 'E10l', 'E106', 'E108', 'E109', 'E110', 'E111', 'E116', 'E118' , 'E119',
								'E120', 'E121', 'E126', 'E128', 'E129', 'E130', 'E131', 'E136', 'E138', 'E139', 
								'E140', 'E141', 'E146', 'E148', 'E149');

# diabetes with chronic complication: 286 codes
select case when icd9_code is null then icd10_code else icd9_code end as DWCC
from d_icd_diagnoses_v
where substr(icd9_code, 1, 4) in ('2504', '2505', '2506', '2507')
or substr(icd10_code, 1, 4) in ('E102', 'E103', 'E104', 'E105', 'E107', 'E112', 'E113', 'E114', 'E115', 'E117',
								'E122', 'E123', 'E124', 'E125', 'E127', 'E132', 'E133', 'E134', 'E135', 'E137', 
                                'E142', 'E143', 'E144', 'E145', 'E147');

# hemiplegia or paraplegia: 93 codes
select case when icd9_code is null then icd10_code else icd9_code end as HP
from d_icd_diagnoses_v
where substr(icd9_code, 1, 3) in ('342', '343')
or substr(icd9_code, 1, 4) in ('3341', '3440', '3441', '3442', '3443', '3444', '3445', '3446', '3449')
or substr(icd10_code, 1, 3) in ('G81', 'G82')
or substr(icd10_code, 1, 4) in ('G041', 'G114', 'G801', 'G802', 'G830', 'G831', 'G832', 'G833', 'G834', 'G839');

# renal disease: 72 codes
select case when icd9_code is null then icd10_code else icd9_code end as renal
from d_icd_diagnoses_v
where substr(icd9_code, 1, 3) in ('582', '585', '586', 'V56')
or substr(icd9_code, 1, 4) in ('5880', 'V420', 'V451')
or substr(icd9_code, 1, 4) between '5830' and '5837'
or substr(icd9_code, 1, 5) in ('40301', '40311', '40391', '40402', '40403', '40412', '40413', '40492', '40493')
or substr(icd10_code, 1, 3) in ('N18', 'N19')
or substr(icd10_code, 1, 4) in ('I120', 'I131', 'N032', 'N033', 'N034', 'N035', 'N036', 'N037', 'N052', 'N053',
								'N054', 'N055', 'N056', 'N057', 'N250', 'Z490', 'Z491', 'Z492', 'Z940', 'Z992');

# any malignancy, including lymphoma and leukemia, except malignant neoplasm of skin: 1843 codes
select case when icd9_code is null then icd10_code else icd9_code end as malignancy
from d_icd_diagnoses_v
where substr(icd9_code, 1, 3) between '140' and '172'
or substr(icd9_code, 1, 4) between '1740' and '1958'
or substr(icd9_code, 1, 3) between '200' and '208'
or substr(icd9_code, 1, 4) = '2386'
or substr(icd10_code, 1, 3) in ('C43', 'C88')
or substr(icd10_code, 1, 3) between 'C00' and 'C26'
or substr(icd10_code, 1, 3) between 'C30' and 'C34'
or substr(icd10_code, 1, 3) between 'C37' and 'C41'
or substr(icd10_code, 1, 3) between 'C45' and 'C58'
or substr(icd10_code, 1, 3) between 'C60' and 'C76'
or substr(icd10_code, 1, 3) between 'C81' and 'C85'
or substr(icd10_code, 1, 3) between 'C90' and 'C97';

# moderate or severe liver disease: 27 codes
select case when icd9_code is null then icd10_code else icd9_code end as SLD
from d_icd_diagnoses_v
where substr(icd9_code, 1, 4) in ('4560', '4561', '4562')
or substr(icd9_code, 1, 4) between '5722' and '5728'
or substr(icd10_code, 1, 4) in ('I850', 'I859', 'I864', 'I982', 'K704', 'K711', 'K721', 'K729', 'K765', 'K766', 'K767');

# metastatic solid tumor: 93 codes
select case when icd9_code is null then icd10_code else icd9_code end as MST
from d_icd_diagnoses_v
where substr(icd9_code, 1, 3) in ('196', '197', '198', '199')
or substr(icd10_code, 1, 3) in ('C77', 'C78', 'C79', 'C80');

# AIDS/HIV: 2 codes
select case when icd9_code is null then icd10_code else icd9_code end as AIDSHIV
from d_icd_diagnoses_v
where substr(icd9_code, 1, 3) in ('042', '043', '044')
or substr(icd10_code, 1, 3) in ('B20', 'B21', 'B22', 'B24');

# putting the above code together, create a temp table
create temporary table icd_filtered
select case when icd9_code is null then icd10_code else icd9_code end as icd_code, 'MI' title
from d_icd_diagnoses_v
where substr(icd9_code, 1, 3) in ('410', '412')
or substr(icd10_code, 1, 3) in ('I21', 'I22')
or substr(icd10_code, 1, 4) = 'I252'
union all 
select case when icd9_code is null then icd10_code else icd9_code end as icd_code, 'CHF' title
from d_icd_diagnoses_v
where substr(icd9_code, 1, 3) = '428'
or substr(icd9_code, 1, 5) in ('39891', '40201', '40211', '40291', '40401', '40403', '40411', '40413', '40491', '40493')
or substr(icd9_code, 1, 4) between '4254' and '4259'
or substr(icd10_code, 1, 3) in ('I43', 'I50')
or substr(icd10_code, 1, 4) in ('I099', 'I110', 'I130', 'I132', 'I255', 'I420', 'I425', 'I426', 'I427', 'I428', 'I429', 'P290')
union all
select case when icd9_code is null then icd10_code else icd9_code end as icd_code, 'PVD' title
from d_icd_diagnoses_v
where substr(icd9_code, 1, 3) in ('440', '441')
or substr(icd9_code, 1, 4) in ('0930', '4373', '4471', '5571', '5579', 'V434')
or substr(icd9_code, 1, 4) between '4431' and '4439'
or substr(icd10_code, 1, 3) in ('I70', 'I71')
or substr(icd10_code, 1, 4) in ('I731', 'I738', 'I739', 'I771', 'I790', 'I792', 'K551', 'K558', 'K559', 'Z958', 'Z959')
union all
select case when icd9_code is null then icd10_code else icd9_code end as icd_code, 'CD' title
from d_icd_diagnoses_v
where substr(icd9_code, 1, 3) between '430' and '438'
or substr(icd9_code, 1, 5) = '36234'
or substr(icd10_code, 1, 3) in ('G45', 'G46')
or substr(icd10_code, 1, 3) between 'I60' and 'I69'
or substr(icd10_code, 1, 4) = 'H340'
union all
select case when icd9_code is null then icd10_code else icd9_code end as icd_code, 'dementia' title
from d_icd_diagnoses_v
where substr(icd9_code, 1, 3) = '290'
or substr(icd9_code, 1, 4) in ('2941', '3312')
or substr(icd10_code, 1, 3) in ('F00', 'F01', 'F02', 'F03', 'G30')
or substr(icd10_code, 1, 4) in ('F051', 'G311')
union all
select case when icd9_code is null then icd10_code else icd9_code end as icd_code, 'CPD' title
from d_icd_diagnoses_v
where substr(icd9_code, 1, 3) between '490' and '505'
or substr(icd9_code, 1, 4) in ('4168', '4169', '5064', '5081', '5088')
or substr(icd10_code, 1, 3) between 'J40' and 'J47'
or substr(icd10_code, 1, 3) between 'J60' and 'J67'
or substr(icd10_code, 1, 4) in ('I278', 'I279', 'J684', 'J701', 'J703')
union all
select case when icd9_code is null then icd10_code else icd9_code end as icd_code, 'rheumatic' title
from d_icd_diagnoses_v
where substr(icd9_code, 1, 3) = '725'
or substr(icd9_code, 1, 4) in ('4465', '7100', '7101', '7102', '7103', '7104', '7140', '7141', '7142', '7148')
or substr(icd10_code, 1, 4) in ('M315', 'M351', 'M353', 'M360')
union all
select case when icd9_code is null then icd10_code else icd9_code end as icd_code, 'PUD' title
from d_icd_diagnoses_v
where substr(icd9_code, 1, 3) in ('531', '532', '533', '534')
or substr(icd10_code, 1, 3) in ('K25', 'K26', 'K27', 'K28')
union all
select case when icd9_code is null then icd10_code else icd9_code end as icd_code, 'MLD' title
from d_icd_diagnoses_v
where substr(icd9_code, 1, 3) in ('570', '571')
or substr(icd9_code, 1, 4) in ('0706', '0709', '5733', '5734', '5738', '5739', 'V427')
or substr(icd9_code, 1, 5) in ('07022', '07023', '07032', '07033', '07044', '07054')
or substr(icd10_code, 1, 3) in ('B18', 'K73', 'K74')
or substr(icd10_code, 1, 4) in ('K700', 'K701', 'K702', 'K703', 'K709', 'K713', 'K714', 'K715', 'K717', 
								'K760', 'K762', 'K763', 'K764', 'K768', 'K769', 'Z944')
union all
select case when icd9_code is null then icd10_code else icd9_code end as icd_code, 'DCC' title
from d_icd_diagnoses_v
where substr(icd9_code, 1, 4) in ('2500', '2501', '2502', '2503', '2508', '2509')
or substr(icd10_code, 1, 4) in ('E100', 'E10l', 'E106', 'E108', 'E109', 'E110', 'E111', 'E116', 'E118' , 'E119',
								'E120', 'E121', 'E126', 'E128', 'E129', 'E130', 'E131', 'E136', 'E138', 'E139', 
								'E140', 'E141', 'E146', 'E148', 'E149')
union all
select case when icd9_code is null then icd10_code else icd9_code end as icd_code, 'DWCC' title
from d_icd_diagnoses_v
where substr(icd9_code, 1, 4) in ('2504', '2505', '2506', '2507')
or substr(icd10_code, 1, 4) in ('E102', 'E103', 'E104', 'E105', 'E107', 'E112', 'E113', 'E114', 'E115', 'E117',
								'E122', 'E123', 'E124', 'E125', 'E127', 'E132', 'E133', 'E134', 'E135', 'E137', 
                                'E142', 'E143', 'E144', 'E145', 'E147')
union all
select case when icd9_code is null then icd10_code else icd9_code end as icd_code, 'HP' title
from d_icd_diagnoses_v
where substr(icd9_code, 1, 3) in ('342', '343')
or substr(icd9_code, 1, 4) in ('3341', '3440', '3441', '3442', '3443', '3444', '3445', '3446', '3449')
or substr(icd10_code, 1, 3) in ('G81', 'G82')
or substr(icd10_code, 1, 4) in ('G041', 'G114', 'G801', 'G802', 'G830', 'G831', 'G832', 'G833', 'G834', 'G839')
union all
select case when icd9_code is null then icd10_code else icd9_code end as icd_code, 'renal' title
from d_icd_diagnoses_v
where substr(icd9_code, 1, 3) in ('582', '585', '586', 'V56')
or substr(icd9_code, 1, 4) in ('5880', 'V420', 'V451')
or substr(icd9_code, 1, 4) between '5830' and '5837'
or substr(icd9_code, 1, 5) in ('40301', '40311', '40391', '40402', '40403', '40412', '40413', '40492', '40493')
or substr(icd10_code, 1, 3) in ('N18', 'N19')
or substr(icd10_code, 1, 4) in ('I120', 'I131', 'N032', 'N033', 'N034', 'N035', 'N036', 'N037', 'N052', 'N053',
								'N054', 'N055', 'N056', 'N057', 'N250', 'Z490', 'Z491', 'Z492', 'Z940', 'Z992')
union all
select case when icd9_code is null then icd10_code else icd9_code end as icd_code, 'malignancy' title
from d_icd_diagnoses_v
where substr(icd9_code, 1, 3) between '140' and '172'
or substr(icd9_code, 1, 4) between '1740' and '1958'
or substr(icd9_code, 1, 3) between '200' and '208'
or substr(icd9_code, 1, 4) = '2386'
or substr(icd10_code, 1, 3) in ('C43', 'C88')
or substr(icd10_code, 1, 3) between 'C00' and 'C26'
or substr(icd10_code, 1, 3) between 'C30' and 'C34'
or substr(icd10_code, 1, 3) between 'C37' and 'C41'
or substr(icd10_code, 1, 3) between 'C45' and 'C58'
or substr(icd10_code, 1, 3) between 'C60' and 'C76'
or substr(icd10_code, 1, 3) between 'C81' and 'C85'
or substr(icd10_code, 1, 3) between 'C90' and 'C97'
union all
select case when icd9_code is null then icd10_code else icd9_code end as icd_code, 'SLD' title
from d_icd_diagnoses_v
where substr(icd9_code, 1, 4) in ('4560', '4561', '4562')
or substr(icd9_code, 1, 4) between '5722' and '5728'
or substr(icd10_code, 1, 4) in ('I850', 'I859', 'I864', 'I982', 'K704', 'K711', 'K721', 'K729', 'K765', 'K766', 'K767')
union all
select case when icd9_code is null then icd10_code else icd9_code end as icd_code, 'MST' title
from d_icd_diagnoses_v
where substr(icd9_code, 1, 3) in ('196', '197', '198', '199')
or substr(icd10_code, 1, 3) in ('C77', 'C78', 'C79', 'C80')
union all
select case when icd9_code is null then icd10_code else icd9_code end as icd_code, 'AIDSHIV' title
from d_icd_diagnoses_v
where substr(icd9_code, 1, 3) in ('042', '043', '044')
or substr(icd10_code, 1, 3) in ('B20', 'B21', 'B22', 'B24')
union all
select icd_code, 'hypertension' title
from d_icd_diagnoses
where long_title like '%hypertensi%'
union all
select icd_code, 'DM' title
from d_icd_diagnoses
where long_title like '%diabetes%' and long_title like '%mellitus%';


-- # hypertension: 263 codes
-- select * from d_icd_diagnoses where long_title like '%hypertensi%';

-- # diabetes mellitus: 669 codes
-- select * from d_icd_diagnoses where long_title like '%diabetes%' and long_title like '%mellitus%';

-- # liver failure: 1 code
-- select * from d_icd_diagnoses where long_title like '%liver%' and long_title like '%failure%' and long_title not like '%deliver%';

-- # renal failure: 20 codes
-- select * from d_icd_diagnoses where long_title like '%renal failure%';

-- # heart failure: 60 codes
-- select * from d_icd_diagnoses where long_title like '%heart failure%' and long_title not like '%without%';

-- # malignancies: 1431 codes
-- select * from d_icd_diagnoses where long_title like '%malign%';

-- # putting the above codes together
-- select icd_code,
-- case 
-- 	when long_title like '%hypertensi%' then 'hypertension'
-- 	when long_title like '%diabetes%' and long_title like '%mellitus%' then 'diabetes mellitus'
--     when long_title like '%liver%' and long_title like '%failure%' and long_title not like '%deliver%' then 'liver failure'
--     when long_title like '%renal failure%' then 'renal failure'
--     when long_title like '%heart failure%' and long_title not like '%without%' then 'heart failure'
--     when long_title like '%malign%' then 'malignancies'
-- end as pre_existing
-- from d_icd_diagnoses
-- where long_title like '%hypertensi%' 
-- or (long_title like '%diabetes%' and long_title like '%mellitus%')
-- or (long_title like '%liver%' and long_title like '%failure%' and long_title not like '%deliver%')
-- or long_title like '%renal failure%'
-- or (long_title like '%heart failure%' and long_title not like '%without%')
-- or long_title like '%malign%';



### admission & edsepsis_admission: identify admissions before current admission: 10725 admissions
select edsepsis_admission.subject_id, admissions.hadm_id
from 
(
	select subject_id, admittime
    from edsepsis_admission
) edsepsis_admission
inner join
(
	select subject_id, hadm_id, admittime
    from admissions
) admissions
on edsepsis_admission.subject_id = admissions.subject_id and edsepsis_admission.admittime > admissions.admittime;



### diagnoses_icd -> find the diganosis of these 10725 admission corresponding to the pre-existing medical contitions
create temporary table pre_existing_details
select before_admission.*, title
from 
(
	select edsepsis_admission.subject_id, admissions.hadm_id
	from 
	(
		select subject_id, admittime
		from edsepsis_admission
	) edsepsis_admission
	inner join
	(
		select subject_id, hadm_id, admittime
		from admissions
	) admissions
	on edsepsis_admission.subject_id = admissions.subject_id and edsepsis_admission.admittime > admissions.admittime
) before_admission
left join
(
	select subject_id, hadm_id, icd_code
    from diagnoses_icd
) diagnoses_icd
on before_admission.hadm_id = diagnoses_icd.hadm_id
inner join
(
	select icd_code, title
	from icd_filtered
) pre_existing_icd
on diagnoses_icd.icd_code = pre_existing_icd.icd_code;

-- create temporary table pre_existing_details
-- select before_admission.*, pre_existing
-- from 
-- (
-- 	select edsepsis_admission.subject_id, admissions.hadm_id
-- 	from 
-- 	(
-- 		select subject_id, admittime
-- 		from edsepsis_admission
-- 	) edsepsis_admission
-- 	inner join
-- 	(
-- 		select subject_id, hadm_id, admittime
-- 		from admissions
-- 	) admissions
-- 	on edsepsis_admission.subject_id = admissions.subject_id and edsepsis_admission.admittime > admissions.admittime
-- ) before_admission
-- left join
-- (
-- 	select subject_id, hadm_id, icd_code
--     from diagnoses_icd
-- ) diagnoses_icd
-- on before_admission.hadm_id = diagnoses_icd.hadm_id
-- inner join
-- (
-- 	select icd_code, 
-- 	case 
-- 		when long_title like '%hypertensi%' then 'hypertension'
-- 		when long_title like '%diabetes%' and long_title like '%mellitus%' then 'diabetes mellitus'
-- 		when long_title like '%liver%' and long_title like '%failure%' and long_title not like '%deliver%' then 'liver failure'
-- 		when long_title like '%renal failure%' then 'renal failure'
-- 		when long_title like '%heart failure%' and long_title not like '%without%' then 'heart failure'
-- 		when long_title like '%malign%' then 'malignancies'
-- 	end as pre_existing
-- 	from d_icd_diagnoses
-- 	where long_title like '%hypertensi%' 
-- 	or (long_title like '%diabetes%' and long_title like '%mellitus%')
-- 	or (long_title like '%liver%' and long_title like '%failure%' and long_title not like '%deliver%')
-- 	or long_title like '%renal failure%'
-- 	or (long_title like '%heart failure%' and long_title not like '%without%')
-- 	or long_title like '%malign%'
-- ) pre_existing_icd
-- on diagnoses_icd.icd_code = pre_existing_icd.icd_code;

# aggregate for each patient (csv)
select stay_id, 
coalesce(MI,0) myocardial_infarction, 
coalesce(CHF,0) congestive_heart_failure, 
coalesce(PVD,0) peripheral_vascular_disease, 
coalesce(CD,0) cerebrovascular_disease, 
coalesce(dementia,0) dementia, 
coalesce(CPD,0) chronic_pulmonary_disease,
coalesce(rheumatic,0) rheumatic_disease,
coalesce(PUD,0) peptic_ulcer_disease,
coalesce(MLD,0) mild_liver_disease,
coalesce(DCC,0) diabetes_without_chronic_complication,
coalesce(DWCC,0) diabetes_with_chronic_complication,
coalesce(HP,0) hemiplegia_or_paraplegia,
coalesce(renal,0) renal_disease,
coalesce(malignancy,0) malignancy,
coalesce(SLD,0) moderate_or_severe_liver_disease,
coalesce(MST,0) metastatic_solid_tumor,
coalesce(AIDSHIV,0) AIDSHIV,
coalesce(hypertension,0) hypertension,
coalesce(DM,0) diabetes_mellitus
from
(
	select subject_id, stay_id
    from edsepsis
) edsepsis
left join
(
	select subject_id, 
	max(MI) MI, 
	max(CHF) CHF, 
	max(PVD) PVD, 
	max(CD) CD, 
	max(dementia) dementia, 
	max(CPD) CPD,
    max(rheumatic) rheumatic,
    max(PUD) PUD,
    max(MLD) MLD,
    max(DCC) DCC,
    max(DWCC) DWCC,
    max(HP) HP,
    max(renal) renal,
    max(malignancy) malignancy,
    max(SLD) SLD,
    max(MST) MST,
    max(AIDSHIV) AIDSHIV,
    max(hypertension) hypertension,
    max(DM) DM
	from
	(
		select subject_id, 
		case when title = 'MI' then 1 else 0 end as MI,
		case when title = 'CHF' then 1 else 0 end as CHF,
		case when title = 'PVD' then 1 else 0 end as PVD,
		case when title = 'CD' then 1 else 0 end as CD,
		case when title = 'dementia' then 1 else 0 end as dementia,
		case when title = 'CPD' then 1 else 0 end as CPD,
        case when title = 'rheumatic' then 1 else 0 end as rheumatic,
        case when title = 'PUD' then 1 else 0 end as PUD,
        case when title = 'MLD' then 1 else 0 end as MLD,
        case when title = 'DCC' then 1 else 0 end as DCC,
        case when title = 'DWCC' then 1 else 0 end as DWCC,
        case when title = 'HP' then 1 else 0 end as HP,
        case when title = 'renal' then 1 else 0 end as renal,
        case when title = 'malignancy' then 1 else 0 end as malignancy,
        case when title = 'SLD' then 1 else 0 end as SLD,
        case when title = 'MST' then 1 else 0 end as MST,
        case when title = 'AIDSHIV' then 1 else 0 end as AIDSHIV,
        case when title = 'hypertension' then 1 else 0 end as hypertension,
        case when title = 'DM' then 1 else 0 end as DM
		from
		(
			select distinct subject_id, title
			from pre_existing_details
		) pre_existing_distinct
	) pre_existing_binary
	group by subject_id
) pre_exisiting_final
on edsepsis.subject_id = pre_exisiting_final.subject_id;

-- select stay_id, 
-- coalesce(hypertension,0) hypertension, 
-- coalesce(diabetes_mellitus,0) diabetes_mellitus, 
-- coalesce(liver_failure,0) liver_failure, 
-- coalesce(renal_failure,0) renal_failure, 
-- coalesce(heart_failure,0) heart_failure, 
-- coalesce(malignancies,0) malignancies
-- from
-- (
-- 	select subject_id, stay_id
--     from edsepsis
-- ) edsepsis
-- left join
-- (
-- 	select subject_id, 
-- 	max(hypertension) hypertension, 
-- 	max(diabetes_mellitus) diabetes_mellitus, 
-- 	max(liver_failure) liver_failure, 
-- 	max(renal_failure) renal_failure, 
-- 	max(heart_failure) heart_failure, 
-- 	max(malignancies) malignancies
-- 	from
-- 	(
-- 		select subject_id, 
-- 		case when pre_existing = 'hypertension' then 1 else 0 end as hypertension,
-- 		case when pre_existing = 'diabetes mellitus' then 1 else 0 end as diabetes_mellitus,
-- 		case when pre_existing = 'liver failure' then 1 else 0 end as liver_failure,
-- 		case when pre_existing = 'renal failure' then 1 else 0 end as renal_failure,
-- 		case when pre_existing = 'heart failure' then 1 else 0 end as heart_failure,
-- 		case when pre_existing = 'malignancies' then 1 else 0 end as malignancies
-- 		from
-- 		(
-- 			select distinct subject_id, pre_existing
-- 			from pre_existing_details
-- 		) pre_existing_distinct
-- 	) pre_existing_binary
-- 	group by subject_id
-- ) pre_exisiting_final
-- on edsepsis.subject_id = pre_exisiting_final.subject_id;



###############
# Vital signs #   
###############
### vitalsign -> temperature, heart rate, respiratory rate, oxygen saturation, systolic and diastolic blood pressure (csv)
# stay_id -> mutlipe charttime, i.e. records
# selecting the median of all records 
# not excluding the ones with charttime > outtime, belonging to same stay_id
-- select edsepsis.stay_id, 
-- median(temperature) temperature_median,
-- median(heartrate) heartrate_median,
-- median(resprate) resprate_median,
-- median(o2sat) o2sat_median,
-- median(sbp) sbp_median,
-- median(dbp) dbp_median
-- from 
-- (
-- 	select stay_id
--     from edsepsis
-- ) edsepsis
-- left join vitalsign
-- on edsepsis.stay_id = vitalsign.stay_id
-- group by edsepsis.stay_id;

# no median fn in mysql
# rank temperature
with temp_numbered as 
(
select edsepsis.stay_id, temperature, count(*) over (partition by edsepsis.stay_id) as cnt,
    row_number() over (partition by edsepsis.stay_id order by temperature) as rownum
from 
(
	select stay_id
    from edsepsis
) edsepsis
left join 
(
	select stay_id, temperature
    from vitalsign
) vitalsign
on edsepsis.stay_id = vitalsign.stay_id
where temperature is not null
),

# rank heart rate
hr_numbered as
(
select edsepsis.stay_id, heartrate, count(*) over (partition by edsepsis.stay_id) as cnt,
    row_number() over (partition by edsepsis.stay_id order by heartrate) as rownum
from 
(
	select stay_id
    from edsepsis
) edsepsis
left join 
(
	select stay_id, heartrate
    from vitalsign
) vitalsign
on edsepsis.stay_id = vitalsign.stay_id
where heartrate is not null
),

# rank respiratory rate
rr_numbered as
(
select edsepsis.stay_id, resprate, count(*) over (partition by edsepsis.stay_id) as cnt,
    row_number() over (partition by edsepsis.stay_id order by resprate) as rownum
from 
(
	select stay_id
    from edsepsis
) edsepsis
left join 
(
	select stay_id, resprate
    from vitalsign
) vitalsign
on edsepsis.stay_id = vitalsign.stay_id
where resprate is not null
),

# rank o2 saturation
os_numbered as
(
select edsepsis.stay_id, o2sat, count(*) over (partition by edsepsis.stay_id) as cnt,
    row_number() over (partition by edsepsis.stay_id order by o2sat) as rownum
from 
(
	select stay_id
    from edsepsis
) edsepsis
left join 
(
	select stay_id, o2sat
    from vitalsign
) vitalsign
on edsepsis.stay_id = vitalsign.stay_id
where o2sat is not null
),

# rank systolic blood pressure
sbp_numbered as
(
select edsepsis.stay_id, sbp, count(*) over (partition by edsepsis.stay_id) as cnt,
    row_number() over (partition by edsepsis.stay_id order by sbp) as rownum
from 
(
	select stay_id
    from edsepsis
) edsepsis
left join 
(
	select stay_id, sbp
    from vitalsign
) vitalsign
on edsepsis.stay_id = vitalsign.stay_id
where sbp is not null
),

# rank diastolic blood pressure
dbp_numbered as
(
select edsepsis.stay_id, dbp, count(*) over (partition by edsepsis.stay_id) as cnt,
    row_number() over (partition by edsepsis.stay_id order by dbp) as rownum
from 
(
	select stay_id
    from edsepsis
) edsepsis
left join 
(
	select stay_id, dbp
    from vitalsign
) vitalsign
on edsepsis.stay_id = vitalsign.stay_id
where dbp is not null
)

select edsepsis.stay_id, temperature temperature_median, heartrate heartrate_median, resprate resprate_median,
o2sat o2sat_median, sbp sbp_median, dbp dbp_median
from edsepsis
left join
(	# temperature
	select stay_id, round(avg(temperature),1) temperature
	from temp_numbered
	where rownum in (cnt/2, (cnt+1)/2, (cnt+2)/2)
    group by stay_id
) temp_median
on edsepsis.stay_id = temp_median.stay_id
left join
(	# heart rate
	select stay_id, round(avg(heartrate),1) heartrate
	from hr_numbered
	where rownum in (cnt/2, (cnt+1)/2, (cnt+2)/2)
    group by stay_id
) hr_median
on edsepsis.stay_id = hr_median.stay_id
left join
(	# respiratory rate
	select stay_id, round(avg(resprate),1) resprate
	from rr_numbered
	where rownum in (cnt/2, (cnt+1)/2, (cnt+2)/2)
    group by stay_id
) rr_median
on edsepsis.stay_id = rr_median.stay_id
left join
(	# o2 saturation
	select stay_id, round(avg(o2sat),1) o2sat
	from os_numbered
	where rownum in (cnt/2, (cnt+1)/2, (cnt+2)/2)
    group by stay_id
) os_median
on edsepsis.stay_id = os_median.stay_id
left join
(	# systolic blood pressure
	select stay_id, round(avg(sbp),1) sbp
	from sbp_numbered
	where rownum in (cnt/2, (cnt+1)/2, (cnt+2)/2)
    group by stay_id
) sbp_median
on edsepsis.stay_id = sbp_median.stay_id
left join
(	# diastolic blood pressure
	select stay_id, round(avg(dbp),1) dbp
	from dbp_numbered
	where rownum in (cnt/2, (cnt+1)/2, (cnt+2)/2)
    group by stay_id
) dbp_median
on edsepsis.stay_id = dbp_median.stay_id;



# getting first vital sign measurement in ED (based on charttime, may contain null values)
select stay_id, temperature temperature_first, heartrate heartrate_first, resprate resprate_first, o2sat o2sat_first, sbp sbp_first, dbp dbp_first
from
(
	select vitalsign.*, 
	row_number() over(partition by stay_id order by charttime) row_num 
	from
	(
		select stay_id 
		from edsepsis
	) edsepsis
	inner join vitalsign
	on edsepsis.stay_id = vitalsign.stay_id
) vitalsign_ranked
where row_num = 1; # several stays' first charts have all null



# putting it all together (csv)
with temp_numbered as 
(
select edsepsis.stay_id, temperature, count(*) over (partition by edsepsis.stay_id) as cnt,
    row_number() over (partition by edsepsis.stay_id order by temperature) as rownum
from 
(
	select stay_id
    from edsepsis
) edsepsis
left join 
(
	select stay_id, temperature
    from vitalsign
) vitalsign
on edsepsis.stay_id = vitalsign.stay_id
where temperature is not null
),
hr_numbered as
(
select edsepsis.stay_id, heartrate, count(*) over (partition by edsepsis.stay_id) as cnt,
    row_number() over (partition by edsepsis.stay_id order by heartrate) as rownum
from 
(
	select stay_id
    from edsepsis
) edsepsis
left join 
(
	select stay_id, heartrate
    from vitalsign
) vitalsign
on edsepsis.stay_id = vitalsign.stay_id
where heartrate is not null
),
rr_numbered as
(
select edsepsis.stay_id, resprate, count(*) over (partition by edsepsis.stay_id) as cnt,
    row_number() over (partition by edsepsis.stay_id order by resprate) as rownum
from 
(
	select stay_id
    from edsepsis
) edsepsis
left join 
(
	select stay_id, resprate
    from vitalsign
) vitalsign
on edsepsis.stay_id = vitalsign.stay_id
where resprate is not null
),
os_numbered as
(
select edsepsis.stay_id, o2sat, count(*) over (partition by edsepsis.stay_id) as cnt,
    row_number() over (partition by edsepsis.stay_id order by o2sat) as rownum
from 
(
	select stay_id
    from edsepsis
) edsepsis
left join 
(
	select stay_id, o2sat
    from vitalsign
) vitalsign
on edsepsis.stay_id = vitalsign.stay_id
where o2sat is not null
),
sbp_numbered as
(
select edsepsis.stay_id, sbp, count(*) over (partition by edsepsis.stay_id) as cnt,
    row_number() over (partition by edsepsis.stay_id order by sbp) as rownum
from 
(
	select stay_id
    from edsepsis
) edsepsis
left join 
(
	select stay_id, sbp
    from vitalsign
) vitalsign
on edsepsis.stay_id = vitalsign.stay_id
where sbp is not null
),
dbp_numbered as
(
select edsepsis.stay_id, dbp, count(*) over (partition by edsepsis.stay_id) as cnt,
    row_number() over (partition by edsepsis.stay_id order by dbp) as rownum
from 
(
	select stay_id
    from edsepsis
) edsepsis
left join 
(
	select stay_id, dbp
    from vitalsign
) vitalsign
on edsepsis.stay_id = vitalsign.stay_id
where dbp is not null
)

select edsepsis.stay_id, temperature temperature_median, heartrate heartrate_median, resprate resprate_median,
o2sat o2sat_median, sbp sbp_median, dbp dbp_median,
temperature_first, heartrate_first, resprate_first, 
o2sat_first, sbp_first, dbp_first
from edsepsis
left join
(	# temperature
	select stay_id, round(avg(temperature),1) temperature
	from temp_numbered
	where rownum in (cnt/2, (cnt+1)/2, (cnt+2)/2)
    group by stay_id
) temp_median
on edsepsis.stay_id = temp_median.stay_id
left join
(	# heart rate
	select stay_id, round(avg(heartrate),1) heartrate
	from hr_numbered
	where rownum in (cnt/2, (cnt+1)/2, (cnt+2)/2)
    group by stay_id
) hr_median
on edsepsis.stay_id = hr_median.stay_id
left join
(	# respiratory rate
	select stay_id, round(avg(resprate),1) resprate
	from rr_numbered
	where rownum in (cnt/2, (cnt+1)/2, (cnt+2)/2)
    group by stay_id
) rr_median
on edsepsis.stay_id = rr_median.stay_id
left join
(	# o2 saturation
	select stay_id, round(avg(o2sat),1) o2sat
	from os_numbered
	where rownum in (cnt/2, (cnt+1)/2, (cnt+2)/2)
    group by stay_id
) os_median
on edsepsis.stay_id = os_median.stay_id
left join
(	# systolic blood pressure
	select stay_id, round(avg(sbp),1) sbp
	from sbp_numbered
	where rownum in (cnt/2, (cnt+1)/2, (cnt+2)/2)
    group by stay_id
) sbp_median
on edsepsis.stay_id = sbp_median.stay_id
left join
(	# diastolic blood pressure
	select stay_id, round(avg(dbp),1) dbp
	from dbp_numbered
	where rownum in (cnt/2, (cnt+1)/2, (cnt+2)/2)
    group by stay_id
) dbp_median
on edsepsis.stay_id = dbp_median.stay_id
left join
(	# first vital sign
	select stay_id, temperature temperature_first, heartrate heartrate_first, resprate resprate_first, 
    o2sat o2sat_first, sbp sbp_first, dbp dbp_first
	from
	(
		select vitalsign.*, 
		row_number() over(partition by stay_id order by charttime) row_num 
		from
		(
			select stay_id 
			from edsepsis
		) edsepsis
		inner join vitalsign
		on edsepsis.stay_id = vitalsign.stay_id
	) vitalsign_ranked
	where row_num = 1
) vital_sign_first
on edsepsis.stay_id = vital_sign_first.stay_id;



####################################
# Biochemical/test related factors #   
####################################
### icustays -> icu intime and outtime: 2537 icu stays
# hadm_id -> multiple ICU stays
# not every hosp admission goes to ICU
select edsepsis_admission.*, icu_stay_id, icu_intime, icu_outtime
from edsepsis_admission
left join
(
	select hadm_id, stay_id icu_stay_id, intime icu_intime, outtime icu_outtime
    from icustays
) icustays
on edsepsis_admission.hadm_id = icustays.hadm_id;

# keep first ICU admission
create table edsepsis_hosp_ICU
select *
from
(
	select edsepsis_admission.*, icu_stay_id, icu_intime, icu_outtime,
	row_number() over(partition by stay_id order by icu_intime) row_num
	from edsepsis_admission
	left join
	(
		select hadm_id, stay_id icu_stay_id, intime icu_intime, outtime icu_outtime
		from icustays
	) icustays
	on edsepsis_admission.hadm_id = icustays.hadm_id
) edsepsis_ICU
where row_num = 1;



### d_labitems -> get itemid for WBC, platelet, CRP, BG, procalcitonin (x), lactate, creatinine and bilirubin
# white blood cells (K/uL)
select * from d_labitems where label like '%white%'; # 51301 (complete_blood_count.sql)

# platelet count (K/uL)
select * from d_labitems where label like '%platelet%'; # 51265 (complete_blood_count.sql)

# C-reactive protein (mg/L)
select * from d_labitems where label like '%protein%' and label like '%C-reactive%'; # 50889 (lab_itemid_to_loinc.csv)

# blood glucose (mg/dL)
select * from d_labitems where label like '%glucose%'; # 50931 (chemistry.sql <=10000); 50809 (bg.sql <=10000)

# procalcitonin (cannot find)
select * from d_labitems where label like '%Calci%';

# lactate (mmol/L)
select * from d_labitems where label like '%lactate%'; # 50813 (bg.sql <=10000)

# creatinine (mg/dL)
select * from d_labitems where label like '%creatinine%'; # 50912 (chemistry.sql <=150)

# bilirubin levels (mg/dL)
select * from d_labitems where label like '%bilirubin%'; # 50885 (enzyme.sql)

# po2 (mm Hg)
select * from d_labitems where label like '%po2%'; # 50821 bg.sql

# fio2 (%)
select * from d_labitems where label like '%oxygen%'; # 50816 bg.sql



### labevents -> get the above tests (taking max by specimen_id, following logic in measurement)
# during ED stay (csv)
select edsepsis.stay_id, 
max(WBC) WBC_ED, 
max(platelet) platelet_ED, 
max(CRP) CRP_ED, 
max(glucose) glucose_ED, 
max(glucose_bg) glucose_bg_ED, 
max(lactate) lactate_ED, 
max(creatinine) creatinine_ED, 
max(bilirubin) bilirubin_ED, 
max(po2) po2_ED, 
max(fio2) fio2_ED
from 
(
	select hadm_id, stay_id, admittime, icu_intime,	icu_outtime
	from
	(
		select edsepsis_admission.*, icu_stay_id, icu_intime, icu_outtime,
		row_number() over(partition by stay_id order by icu_intime) row_num
		from edsepsis_admission
		left join
		(
			select hadm_id, stay_id icu_stay_id, intime icu_intime, outtime icu_outtime
			from icustays
		) icustays
		on edsepsis_admission.hadm_id = icustays.hadm_id
	) edsepsis_ICU
	where row_num = 1
) edsepsis
left join
(
	select max(hadm_id) as hadm_id, max(charttime) as charttime,
	max(case when itemid = 51301 then valuenum else null end) as WBC,
	max(case when itemid = 51265 then valuenum else null end) as platelet,
	max(case when itemid = 50889 then valuenum else null end) as CRP,
	max(case when itemid = 50931 then valuenum else null end) as glucose,
	max(case when itemid = 50809 then valuenum else null end) as glucose_bg,
	max(case when itemid = 50813 then valuenum else null end) as lactate,
	max(case when itemid = 50912 then valuenum else null end) as creatinine,
	max(case when itemid = 50885 then valuenum else null end) as bilirubin,
	max(case when itemid = 50821 then valuenum else null end) as po2,
	max(case when itemid = 50816 then valuenum else null end) as fio2
	from labevents
	where itemid in (51301, 51265, 50889, 50931, 50809, 50813, 50912, 50885, 50821, 50816) 
	and valuenum is not null and valuenum > 0
	group by specimen_id
) labevents
on edsepsis.hadm_id = labevents.hadm_id and edsepsis.intime <= labevents.charttime and edsepsis.outtime >= labevents.charttime
group by edsepsis.stay_id
order by stay_id;

# create temporary table to store all lab results for ed sepsis patients
create table edsepsis_hosp_ICU_labevents
select *
from edsepsis_hosp_ICU
left join
(
	select max(hadm_id) as lab_hadm_id, max(charttime) as charttime,
	max(case when itemid = 51301 then valuenum else null end) as WBC,
	max(case when itemid = 51265 then valuenum else null end) as platelet,
	max(case when itemid = 50889 then valuenum else null end) as CRP,
	max(case when itemid = 50931 then valuenum else null end) as glucose,
	max(case when itemid = 50809 then valuenum else null end) as glucose_bg,
	max(case when itemid = 50813 then valuenum else null end) as lactate,
	max(case when itemid = 50912 then valuenum else null end) as creatinine,
	max(case when itemid = 50885 then valuenum else null end) as bilirubin,
	max(case when itemid = 50821 then valuenum else null end) as po2,
	max(case when itemid = 50816 then valuenum else null end) as fio2
	from labevents
	where itemid in (51301, 51265, 50889, 50931, 50809, 50813, 50912, 50885, 50821, 50816) 
	and valuenum is not null and valuenum > 0
	group by specimen_id
) labevents
on edsepsis_hosp_ICU.hadm_id = labevents.lab_hadm_id;



# during hosp (within 1 day of hosp admission) (csv)
select edsepsis_hosp_ICU.stay_id, WBC_hosp, platelet_hosp, CRP_hosp, glucose_hosp, glucose_bg_hosp, lactate_hosp, 
creatinine_hosp, bilirubin_hosp, po2_hosp, fio2_hosp
from edsepsis_hosp_ICU
left join
(
	select stay_id,
	max(WBC) WBC_hosp, 
	max(platelet) platelet_hosp, 
	max(CRP) CRP_hosp, 
	max(glucose) glucose_hosp, 
	max(glucose_bg) glucose_bg_hosp, 
	max(lactate) lactate_hosp, 
	max(creatinine) creatinine_hosp, 
	max(bilirubin) bilirubin_hosp, 
	max(po2) po2_hosp, 
	max(fio2) fio2_hosp
	from edsepsis_hosp_ICU_labevents
	where icu_intime is null and admittime <= charttime and date_add(admittime, interval 1 day) >= charttime # no ICU admission
	group by stay_id
	union all
	select stay_id,
	max(WBC) WBC_hosp, 
	max(platelet) platelet_hosp, 
	max(CRP) CRP_hosp, 
	max(glucose) glucose_hosp, 
	max(glucose_bg) glucose_bg_hosp, 
	max(lactate) lactate_hosp, 
	max(creatinine) creatinine_hosp, 
	max(bilirubin) bilirubin_hosp, 
	max(po2) po2_hosp, 
	max(fio2) fio2_hosp
	from edsepsis_hosp_ICU_labevents
	where icu_intime is not null and timestampdiff(day, admittime, icu_intime) = 0 # with ICU admission, hosp - ICU less than 1 day
	and admittime <= charttime and icu_intime > charttime
	group by stay_id
	union all
	select stay_id,
	max(WBC) WBC_hosp, 
	max(platelet) platelet_hosp, 
	max(CRP) CRP_hosp, 
	max(glucose) glucose_hosp, 
	max(glucose_bg) glucose_bg_hosp, 
	max(lactate) lactate_hosp, 
	max(creatinine) creatinine_hosp, 
	max(bilirubin) bilirubin_hosp, 
	max(po2) po2_hosp, 
	max(fio2) fio2_hosp
	from edsepsis_hosp_ICU_labevents
	where icu_intime is not null and timestampdiff(day, admittime, icu_intime) > 0 # with ICU admission, hosp - ICU equal or more than 1 day
	and admittime <= charttime and date_add(admittime, interval 1 day) >= charttime
	group by stay_id
) labevents_hosp
on edsepsis_hosp_ICU.stay_id = labevents_hosp.stay_id
order by edsepsis_hosp_ICU.stay_id;



# create table for fio2 in chartevents (ICU module)
create table fio2_CE
select *
from edsepsis_hosp_ICU
left join
(
	select hadm_id as CE_hadm_id, stay_id as CE_icu_stay_id, charttime,
	max(case when valuenum > 0.2 and valuenum <= 1 then valuenum * 100
			 when valuenum > 1 and valuenum < 20 then null
			 when valuenum >= 20 and valuenum <= 100 then valuenum
			 else null end) as fio2_chartevents
	from chartevents
	where itemid = 223835 -- Inspired O2 Fraction (FiO2)
	and valuenum > 0 and valuenum <= 100 and stay_id in (select icu_stay_id from edsepsis_hosp_ICU)
	group by hadm_id, CE_icu_stay_id, charttime
) fio2_chartevents
on edsepsis_hosp_ICU.hadm_id = fio2_chartevents.CE_hadm_id;



# during ICU (within 1 day of ICU admission) (csv)
# gcs follows gcs.sql
with gcs_base as
(
    select subject_id, stay_id, charttime,
		   max(case when itemid = 223901 then valuenum else null end) as gcsmotor,
           max(case when itemid = 223900 and value = 'No Response-ETT' then 0
				    when itemid = 223900 then valuenum
                    else null
               end) as gcsverbal,
           max(case when itemid = 220739 then valuenum else null end) as gcseyes,
           row_number() over(partition by stay_id order by charttime asc) as rn
    from chartevents
    where itemid in (223900, 223901, 220739) and stay_id in (select icu_stay_id from edsepsis_hosp_ICU)
    group by subject_id, stay_id, charttime
),

gcs as
(
select b.*, b2.gcsverbal as gcsverbalprev, b2.gcsmotor as gcsmotorprev, b2.gcseyes as gcseyesprev, # the prevs are all null
case when b.gcsverbal = 0 then 15 
	 when b.gcsverbal is null and b2.gcsverbal = 0 then 15
	 when b2.gcsverbal = 0 then coalesce(b.gcsmotor, 6) + coalesce(b.gcsverbal, 5) + coalesce(b.gcseyes, 4)
	 else coalesce(b.gcsmotor, coalesce(b2.gcsmotor, 6)) 
		+ coalesce(b.gcsverbal, coalesce(b2.gcsverbal, 5)) 
        + coalesce(b.gcseyes, coalesce(b2.gcseyes, 4))
	 end as gcs
from gcs_base b
-- join to itself within 6 hours to get previous value
left join gcs_base b2
on b.stay_id = b2.stay_id and b.rn = b2.rn + 1 and b2.charttime > date_add(b.charttime, interval '6' hour)
),

gcs_timeinfo as
(
select edsepsis_hosp_ICU.stay_id, icu_intime, icu_outtime, charttime, gcs
from edsepsis_hosp_ICU
left join gcs
on edsepsis_hosp_ICU.icu_stay_id = gcs.stay_id
)

select edsepsis_hosp_ICU.stay_id, WBC_ICU, platelet_ICU, CRP_ICU, glucose_ICU, glucose_bg_ICU, lactate_ICU, 
creatinine_ICU, bilirubin_ICU, po2_ICU, fio2_ICU, fio2_chartevents, gcs
from edsepsis_hosp_ICU
left join # all tests
(
	select stay_id,
	max(WBC) WBC_ICU, 
	max(platelet) platelet_ICU, 
	max(CRP) CRP_ICU, 
	max(glucose) glucose_ICU, 
	max(glucose_bg) glucose_bg_ICU, 
	max(lactate) lactate_ICU, 
	max(creatinine) creatinine_ICU, 
	max(bilirubin) bilirubin_ICU, 
	max(po2) po2_ICU, 
	max(fio2) fio2_ICU
	from edsepsis_hosp_ICU_labevents
	where icu_intime is not null and timestampdiff(day, icu_intime, icu_outtime) = 0 # with ICU admission, in ICU less than 1 day
	and icu_intime <= charttime and icu_outtime > charttime
	group by stay_id
    union all
    select stay_id,
	max(WBC) WBC_ICU, 
	max(platelet) platelet_ICU, 
	max(CRP) CRP_ICU, 
	max(glucose) glucose_ICU, 
	max(glucose_bg) glucose_bg_ICU, 
	max(lactate) lactate_ICU, 
	max(creatinine) creatinine_ICU, 
	max(bilirubin) bilirubin_ICU, 
	max(po2) po2_ICU, 
	max(fio2) fio2_ICU
	from edsepsis_hosp_ICU_labevents
	where icu_intime is not null and timestampdiff(day, icu_intime, icu_outtime) > 0 # with ICU admission, in ICU more than 1 day
	and icu_intime <= charttime and date_add(icu_intime, interval 1 day) >= charttime
	group by stay_id
) labevents_ICU
on edsepsis_hosp_ICU.stay_id = labevents_ICU.stay_id
left join # fio2 in chartevents
(
	select stay_id,
	max(fio2_chartevents) fio2_chartevents
	from fio2_CE
	where icu_intime is not null and timestampdiff(day, icu_intime, icu_outtime) = 0 # with ICU admission, in ICU less than 1 day
	and icu_intime <= charttime and icu_outtime > charttime
	group by stay_id
    union all
    select stay_id,
	max(fio2_chartevents) fio2_chartevents
	from fio2_CE
	where icu_intime is not null and timestampdiff(day, icu_intime, icu_outtime) > 0 # with ICU admission, in ICU more than 1 day
	and icu_intime <= charttime and date_add(icu_intime, interval 1 day) >= charttime
	group by stay_id
) fio2_ICU # max(fio2) in chartevents and max(fio2) in labevents may differ
on edsepsis_hosp_ICU.stay_id = fio2_ICU.stay_id
left join # gcs in chartevents
(
	select stay_id,
	max(gcs) gcs
	from gcs_timeinfo
	where icu_intime is not null and timestampdiff(day, icu_intime, icu_outtime) = 0 # with ICU admission, in ICU less than 1 day
	and icu_intime <= charttime and icu_outtime > charttime
	group by stay_id
    union all
    select stay_id,
	max(gcs) gcs
	from gcs_timeinfo
	where icu_intime is not null and timestampdiff(day, icu_intime, icu_outtime) > 0 # with ICU admission, in ICU more than 1 day
	and icu_intime <= charttime and date_add(icu_intime, interval 1 day) >= charttime
	group by stay_id
) gcs_ICU # max(fio2) in chartevents and max(fio2) in labevents may differ
on edsepsis_hosp_ICU.stay_id = gcs_ICU.stay_id
order by edsepsis_hosp_ICU.stay_id;



######################
# Use of Vasopressor #   
######################
### inputevents/d_items -> epinephrine, norepinephrine, dopamine, dobutamine
# epinephrine 221289
select * from d_items where itemid = 221289;
select * 
from inputevents
where itemid = 221289 and stay_id in (select icu_stay_id from edsepsis_hosp_ICU);

# norepinephrine 221906
select * from d_items where itemid = 221906;

# dopamine 221662
select * from d_items where itemid = 221662;

# dobutamine 221653
select * from d_items where itemid = 221653;

# put it all together
select edsepsis_hosp_ICU.stay_id, epinephrine_ICU, norepinephrine_ICU, dopamine_ICU, dobutamine_ICU
from 
(
	select stay_id, icu_stay_id
    from edsepsis_hosp_ICU
) edsepsis_hosp_ICU
left join
(
	select stay_id icu_stay_id,
	max(epinephrine_ICU) epinephrine_ICU,
	max(norepinephrine_ICU) norepinephrine_ICU,
	max(dopamine_ICU) dopamine_ICU,
	max(dobutamine_ICU) dobutamine_ICU
	from
	(
		select stay_id, 
		case when itemid = 221289 then sum(amount) end as epinephrine_ICU,
		case when itemid = 221906 then sum(amount) end as norepinephrine_ICU,
		case when itemid = 221662 then sum(amount) end as dopamine_ICU,
		case when itemid = 221653 then sum(amount) end as dobutamine_ICU
		from inputevents
		where itemid in (221289, 221906, 221662, 221653)
		and stay_id in (select icu_stay_id from edsepsis_hosp_ICU)
		group by stay_id, itemid
	) vasopressor_ICU
	group by ICU_stay_id
) vasopressor_ICU_agg
on edsepsis_hosp_ICU.icu_stay_id = vasopressor_ICU_agg.icu_stay_id;



### prescriptions -> epinephrine, norepinephrine, dopamine, dobutamine
# epinephrine
select distinct(drug) from prescriptions where drug like 'epinephrine%' 
and hadm_id in (select hadm_id from edsepsis);

# norepinephrine
select distinct(drug) from prescriptions where drug like 'norepinephrine%' and drug not like 'norepinephrine bitartrate'
and hadm_id in (select hadm_id from edsepsis);

# dopamine
select distinct(drug) from prescriptions where drug like '%dopamine%' 
and hadm_id in (select hadm_id from edsepsis);

# dobutamine
select distinct(drug) from prescriptions where drug like '%dobutamine%' 
and hadm_id in (select hadm_id from edsepsis);

# get all drug usage during ed and hosp and ICU stay (unit: mg)
select stay_id, epinephrine, norepinephrine, dopamine, dobutamine
from 
(
	select hadm_id, stay_id
    from edsepsis
) edsepsis
left join
(
	select hadm_id,
	max(epinephrine) epinephrine,
	max(norepinephrine) norepinephrine,
	max(dopamine) dopamine,
	max(dobutamine) dobutamine
	from 
	(
		select hadm_id,
		case when drug like 'epinephrine%' then sum(dose_val_rx) end as epinephrine,
		case when drug like 'norepinephrine%' and drug not like 'norepinephrine bitartrate' then sum(dose_val_rx) end as norepinephrine,
		case when drug like '%dopamine%' then sum(dose_val_rx) end as dopamine,
		case when drug like '%dobutamine%' then sum(dose_val_rx) end as dobutamine
		from prescriptions
		where hadm_id in (select hadm_id from edsepsis) and dose_val_rx is not null
		and
		(
			drug like 'epinephrine%' 
			or (drug like 'norepinephrine%' and drug not like 'norepinephrine bitartrate')
			or drug like '%dopamine%'
			or drug like '%dobutamine%' 
		)
		group by hadm_id, drug
	) vasopressor
	group by hadm_id
) vasopressor_agg
on edsepsis.hadm_id = vasopressor_agg.hadm_id;



##########################
# Mechanical Ventilation #   
##########################
# oxygen_delivery.sql
with ce1 as (
select
	subject_id, stay_id, charttime,
    -- merge o2 flows into a single row
	case when itemid in (223834, 227582) then 223834 else itemid end as itemid,
    value, valuenum, 
    row_number() over(partition by subject_id, charttime, itemid order by storetime desc) as rn
from chartevents 
where value is not null 
and itemid in
	(223834, -- o2 flow
	 227582, -- bipap o2 flow
	 227287 -- additional o2 flow
	)
and stay_id in (select icu_stay_id from edsepsis_hosp_ICU)
),

o2 as (
    -- The below ITEMID can have multiple entries for charttime/storetime
    -- These are valid entries, and should be retained in derived tables.
    --   224181 -- Small Volume Neb Drug #1       | Respiratory | Text
    -- , 227570 -- Small Volume Neb Drug/Dose #1  | Respiratory | Text
    -- , 224833 -- SBT Deferred                   | Respiratory | Text
    -- , 224716 -- SBT Stopped                    | Respiratory | Text
    -- , 224740 -- RSBI Deferred                  | Respiratory | Text
    -- , 224829 -- Trach Tube Type                | Respiratory | Text
    -- , 226732 -- O2 Delivery Device(s)          | Respiratory | Text
    -- , 226873 -- Inspiratory Ratio              | Respiratory | Numeric
    -- , 226871 -- Expiratory Ratio               | Respiratory | Numeric
    -- maximum of 4 o2 devices on at once
select
	subject_id, stay_id, charttime, itemid, value as o2_device,
    row_number() over(partition by subject_id, charttime, itemid order by value) as rn
from chartevents
where itemid = 226732 -- oxygen delivery device(s)
and stay_id in (select icu_stay_id from edsepsis_hosp_ICU)
),

stg as (
select 
coalesce(ce.subject_id, o2.subject_id) as subject_id,
coalesce(ce.stay_id, o2.stay_id) as stay_id,
coalesce(ce.charttime, o2.charttime) as charttime,
coalesce(ce.itemid, o2.itemid) as itemid,
ce.value, ce.valuenum, o2.o2_device, o2.rn
from ce1 ce
left join o2
on ce.subject_id = o2.subject_id and ce.charttime = o2.charttime
where ce.rn = 1
union # no full outer join in mysql, use left join + right join
select 
coalesce(ce.subject_id, o2.subject_id) as subject_id,
coalesce(ce.stay_id, o2.stay_id) as stay_id,
coalesce(ce.charttime, o2.charttime) as charttime,
coalesce(ce.itemid, o2.itemid) as itemid,
ce.value, ce.valuenum, o2.o2_device, o2.rn
from ce1 ce
right join o2
on ce.subject_id = o2.subject_id and ce.charttime = o2.charttime
where ce.rn = 1
),

oxygen_delivery as
(
select subject_id, max(stay_id) stay_id, charttime,
max(case when itemid = 223834 then valuenum else null end) as o2_flow,
max(case when itemid = 227287 then valuenum else null end) as o2_flow_additional,
-- ensure we retain all o2 devices for the patient
max(case when rn = 1 then o2_device else null end) as o2_delivery_device_1,
max(case when rn = 2 then o2_device else null end) as o2_delivery_device_2,
max(case when rn = 3 then o2_device else null end) as o2_delivery_device_3,
max(case when rn = 4 then o2_device else null end) as o2_delivery_device_4
from stg
group by subject_id, charttime
),

# ventilation_setting.sql
ce2 as (
select subject_id, stay_id, charttime, itemid, value,
-- begin fio2 cleaning
case when itemid = 223835 then case when valuenum >= 0.20 and valuenum <= 1 then valuenum * 100				
									when valuenum > 1 and valuenum < 20 then null
									when valuenum >= 20 and valuenum <= 100 then valuenum
									else null end
-- end of fio2 cleaning
-- begin peep cleaning
     when itemid in (220339, 224700) then case when valuenum > 100 then null
											   when valuenum < 0 then null
											   else valuenum end
-- end peep cleaning
	 else valuenum end as valuenum,
valueuom
from chartevents
where value is not null and stay_id IS NOT NULL
and itemid in (224688, -- Respiratory Rate (Set)
			   224689, -- Respiratory Rate (spontaneous)
			   224690, -- Respiratory Rate (Total)
			   224687, -- minute volume
			   224685, 224684, 224686, -- tidal volume
		       224696, -- PlateauPressure
		       220339, 224700, -- PEEP
		       223835, -- fio2
		       223849, -- vent mode
		       229314, -- vent mode (Hamilton)
		       223848, -- vent type
	           224691) -- Flow Rate (L)
and stay_id in (select icu_stay_id from edsepsis_hosp_ICU)
),

ventilator_setting as
(
select subject_id, max(stay_id) stay_id, charttime,
max(case when itemid = 224688 then valuenum else null end) respiratory_rate_set, 
max(case when itemid = 224690 then valuenum else null end) respiratory_rate_total,
max(case when itemid = 224689 then valuenum else null end) respiratory_rate_spontaneous,
max(case when itemid = 224687 then valuenum else null end) minute_volume,
max(case when itemid = 224684 then valuenum else null end) tidal_volume_set,
max(case when itemid = 224685 then valuenum else null end) tidal_volume_observed,
max(case when itemid = 224686 then valuenum else null end) tidal_volume_spontaneous,
max(case when itemid = 224696 then valuenum else null end) plateau_pressure,
max(case when itemid in (220339, 224700) then valuenum else null end) peep,
max(case when itemid = 223835 then valuenum else null end) fio2,
max(case when itemid = 224691 then valuenum else null end) flow_rate,
max(case when itemid = 223849 then value else null end) ventilator_mode,
max(case when itemid = 229314 then value else null end) ventilator_mode_hamilton,
max(case when itemid = 223848 then value else null end) ventilator_type
from ce2
group BY subject_id, charttime
),

# ventilation.sql
tm as
(
select stay_id, charttime
from ventilator_setting
union distinct
select stay_id, charttime
from oxygen_delivery
),

vs as
(
select tm.stay_id, tm.charttime, o2_delivery_device_1,
coalesce(ventilator_mode, ventilator_mode_hamilton) vent_mode,
-- case statement determining the type of intervention
-- done in order of priority: trach > mech vent > NIV > high flow > o2
case when o2_delivery_device_1 in ('Tracheostomy tube', 'Trach mask') then 'Tracheostomy' -- tracheostomy
	 when o2_delivery_device_1 in ('Endotracheal tube') 
     or ventilator_mode in ('(S) CMV', 'APRV', 'APRV/Biphasic+ApnPress', 'APRV/Biphasic+ApnVol', 'APV (cmv)', 'Ambient', 'Apnea Ventilation',
						    'CMV', 'CMV/ASSIST', 'CMV/ASSIST/AutoFlow', 'CMV/AutoFlow', 'CPAP/PPS', 'CPAP/PSV', 'CPAP/PSV+Apn TCPL',
                            'CPAP/PSV+ApnPres', 'CPAP/PSV+ApnVol', 'MMV', 'MMV/AutoFlow', 'MMV/PSV', 'MMV/PSV/AutoFlow', 'P-CMV',
                            'PCV+', 'PCV+/PSV', 'PCV+Assist', 'PRES/AC', 'PRVC/AC', 'PRVC/SIMV', 'PSV/SBT', 'SIMV', 'SIMV/AutoFlow',
                            'SIMV/PRES', 'SIMV/PSV', 'SIMV/PSV/AutoFlow', 'SIMV/VOL', 'SYNCHRON MASTER', 'SYNCHRON SLAVE', 'VOL/AC')
	 or ventilator_mode_hamilton in ('APRV', 'APV (cmv)', 'Ambient', '(S) CMV', 'P-CMV', 'SIMV', 'APV (simv)', 'P-SIMV', 'VS', 'ASV')
	 then 'InvasiveVent' -- mechanical / invasive ventilation
     when o2_delivery_device_1 in ('Bipap mask', 'CPAP mask')
     or ventilator_mode_hamilton in ('DuoPaP', 'NIV', 'NIV-ST')
	 then 'NonInvasiveVent' -- NIV
     when o2_delivery_device_1 = 'High flow nasal cannula' then 'HFNC' -- high flow nasal cannula (not to consider)
     when o2_delivery_device_1 in ('Non-rebreather' , 'Face tent' , 'Aerosol-cool' , 'Venti mask' , 'Medium conc mask',
								   'Ultrasonic neb' , 'Vapomist' , 'Oxymizer' , 'High flow neb' , 'Nasal cannula')
     then 'SupplementalOxygen' -- non rebreather (not to consider)
	 when o2_delivery_device_1 = 'None' then 'None'
	 else null end as ventilation_status     
from tm
left join ventilator_setting
on tm.stay_id = ventilator_setting.stay_id and tm.charttime = ventilator_setting.charttime
left join oxygen_delivery
on tm.stay_id = oxygen_delivery.stay_id and tm.charttime = oxygen_delivery.charttime
)

select stay_id, edsepsis_hosp_ICU.icu_stay_id, tracheostomy, IV, NIV, HFNC, supplemental_oxygen
from 
(
	select stay_id, icu_stay_id
    from edsepsis_hosp_ICU
) edsepsis_hosp_ICU
left join
(
	select stay_id icu_stay_id,
	max(tracheostomy) tracheostomy,
	max(IV) IV,
	max(NIV) NIV,
	max(HFNC) HFNC,
	max(supplemental_oxygen) supplemental_oxygen
	from
	(
		select stay_id,
		case when ventilation_status = 'Tracheostomy' then 1 else 0 end as tracheostomy,
		case when ventilation_status = 'InvasiveVent' then 1 else 0 end as IV,
		case when ventilation_status = 'NonInvasiveVent' then 1 else 0 end as NIV,
		case when ventilation_status = 'HFNC' then 1 else 0 end as HFNC,
		case when ventilation_status = 'SupplementalOxygen' then 1 else 0 end as supplemental_oxygen
		from
		(
			select distinct stay_id, ventilation_status
			from vs
			where ventilation_status is not null and ventilation_status != 'None'
		) vent_detail
	) vent
	group by stay_id
) vent_agg
on edsepsis_hosp_ICU.icu_stay_id = vent_agg.icu_stay_id;



###################
# Previous Visits #   
###################
### admission & edsepsis_admission: identify admissions before current admission: 10725 admissions
select stay_id, coalesce(num_admissions, 0) num_admissions 
from 
(
	select subject_id, stay_id
    from edsepsis
) edsepsis
left join
(
	select edsepsis_admission.subject_id, count(admissions.hadm_id) num_admissions
	from 
	(
		select subject_id, admittime
		from edsepsis_admission
	) edsepsis_admission
	inner join
	(
		select subject_id, hadm_id, admittime
		from admissions
	) admissions
	on edsepsis_admission.subject_id = admissions.subject_id and edsepsis_admission.admittime > admissions.admittime
	group by subject_id
) prev_admissions
on edsepsis.subject_id = prev_admissions.subject_id;



### edstays & edsepsis: identify previous ED visits
select stay_id, coalesce(num_ED,0) num_ED, coalesce(num_ED_admissions,0) num_ED_admissions
from 
(
	select subject_id, stay_id
	from edsepsis
) edsepsis
left join
(
	select edsepsis.subject_id, count(stay_id) num_ED, count(hadm_id) num_ED_admissions
	from 
	(
		select subject_id, intime
		from edsepsis
	) edsepsis
	inner join
	(
		select subject_id, hadm_id, stay_id, intime
		from edstays
	) edstays
	on edsepsis.subject_id = edstays.subject_id and edsepsis.intime > edstays.intime
	group by subject_id
) prev_ED
on edsepsis.subject_id = prev_ED.subject_id;

# put it all together
select stay_id, 
coalesce(num_admissions, 0) num_admissions,
coalesce(num_ED,0) num_ED, 
coalesce(num_ED_admissions,0) num_ED_admissions
from 
(
	select subject_id, stay_id
    from edsepsis
) edsepsis
left join
(
	select edsepsis_admission.subject_id, count(admissions.hadm_id) num_admissions
	from 
	(
		select subject_id, admittime
		from edsepsis_admission
	) edsepsis_admission
	inner join
	(
		select subject_id, hadm_id, admittime
		from admissions
	) admissions
	on edsepsis_admission.subject_id = admissions.subject_id and edsepsis_admission.admittime > admissions.admittime
	group by subject_id
) prev_admissions
on edsepsis.subject_id = prev_admissions.subject_id
left join 
(
	select edsepsis.subject_id, count(stay_id) num_ED, count(hadm_id) num_ED_admissions
	from 
	(
		select subject_id, intime
		from edsepsis
	) edsepsis
	inner join
	(
		select subject_id, hadm_id, stay_id, intime
		from edstays
	) edstays
	on edsepsis.subject_id = edstays.subject_id and edsepsis.intime > edstays.intime
	group by subject_id
) prev_ED
on edsepsis.subject_id = prev_ED.subject_id;

select * from edsepsis_hosp_ICU;