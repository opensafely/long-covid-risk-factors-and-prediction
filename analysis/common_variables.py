from cohortextractor import patients
from codelists import *

demographic_variables = dict(
    cov_num_age = patients.age_as_of(
        "index_date",
        return_expectations = {
        "rate": "universal",
        "int": {"distribution": "population_ages"},
        "incidence" : 0.001
        },
    ),
    cov_cat_sex=patients.sex(
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"M": 0.49, "F": 0.51}},
        }
    ),
    cov_cat_region=patients.registered_practice_as_of(
        "index_date",
        returning="nuts1_region_name",
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                    "North East": 0.1,
                    "North West": 0.1,
                    "Yorkshire and The Humber": 0.1,
                    "East Midlands": 0.1,
                    "West Midlands": 0.1,
                    "East": 0.1,
                    "London": 0.2,
                    "South East": 0.1,
                    "South West": 0.1,
                },
            },
        },
    ),
     ## Deprivation
    cov_cat_imd=patients.categorised_as(
        {
            "0": "DEFAULT",
            "1": """index_of_multiple_deprivation >=1 AND index_of_multiple_deprivation < 32844*1/5""",
            "2": """index_of_multiple_deprivation >= 32844*1/5 AND index_of_multiple_deprivation < 32844*2/5""",
            "3": """index_of_multiple_deprivation >= 32844*2/5 AND index_of_multiple_deprivation < 32844*3/5""",
            "4": """index_of_multiple_deprivation >= 32844*3/5 AND index_of_multiple_deprivation < 32844*4/5""",
            "5": """index_of_multiple_deprivation >= 32844*4/5 AND index_of_multiple_deprivation < 32844""",
        },
        index_of_multiple_deprivation=patients.address_as_of(
            "index_date",
            returning="index_of_multiple_deprivation",
            round_to_nearest=100,
        ),
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                    "0": 0.05,
                    "1": 0.19,
                    "2": 0.19,
                    "3": 0.19,
                    "4": 0.19,
                    "5": 0.19,
                }
            },
        },
    ),
    cov_cat_ethnicity=patients.categorised_as(
        {
            "Missing": "DEFAULT",
            "White": """ ethnicity_code=1 """,
            "Mixed": """ ethnicity_code=2 """,
            "South Asian": """ ethnicity_code=3 """,
            "Black": """ ethnicity_code=4 """,
            "Other": """ ethnicity_code=5 """,
        },
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                    "Missing": 0.4,
                    "White": 0.2,
                    "Mixed": 0.1,
                    "South Asian": 0.1,
                    "Black": 0.1,
                    "Other": 0.1,
                }
            },
        },
        ethnicity_code=patients.with_these_clinical_events(
            ethnicity_codes,
            returning="category",
            find_last_match_in_period=True,
            on_or_before="index_date",
            return_expectations={
            "category": {"ratios": {"1": 0.4, "2": 0.4, "3": 0.2, "4":0.2,"5": 0.2}},
            "incidence": 0.75,
            },
        ),
    ),
    ## Healthcare worker    
    cov_cat_healthcare_worker=patients.with_healthcare_worker_flag_on_covid_vaccine_record(
            returning='binary_flag', 
            return_expectations={"incidence": 0.01},
    ),
    # # Question: Not quite sure whether this is covid history before index date?
    # sub_cat_previous_covid=patients.categorised_as(
    #     {
    #         "COVID positive": """
    #                             (sgss_positive OR primary_care_covid)
    #                             AND NOT hospital_covid
    #                             """,
    #         "COVID hospitalised": "hospital_covid",
    #         "No COVID code": "DEFAULT",
    #     },
    #     return_expectations={
    #         "incidence": 1,
    #         "category": {
    #             "ratios": {
    #                 "COVID positive": 0.4,
    #                 "COVID hospitalised": 0.4,
    #                 "No COVID code": 0.2,
    #             }
    #         },
    #     },
    # ),
)

clinical_variables = dict(
    cov_cat_bmi=patients.categorised_as(
        {
            "Not obese": "DEFAULT",
            "Obese I (30-34.9)": """ bmi_value >= 30 AND bmi_value < 35""",
            "Obese II (35-39.9)": """ bmi_value >= 35 AND bmi_value < 40""",
            "Obese III (40+)": """ bmi_value >= 40 AND bmi_value < 100""",
            # set maximum to avoid any impossibly extreme values being classified as obese
        },
        bmi_value=patients.most_recent_bmi(
            on_or_before="index_date - 1 day", minimum_age_at_measurement=16
        ),
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                    "Not obese": 0.7,
                    "Obese I (30-34.9)": 0.1,
                    "Obese II (35-39.9)": 0.1,
                    "Obese III (40+)": 0.1,
                }
            },
        },
    ),
    cov_cat_diabetes=patients.with_these_clinical_events(
        diabetes_codes, on_or_before="index_date - 1 day"
    ),
    cov_cat_cancer=patients.satisfying(
        "other_cancer OR lung_cancer",
        other_cancer=patients.with_these_clinical_events(
            other_cancer_codes, on_or_before="index_date - 1 day"
        ),
        lung_cancer=patients.with_these_clinical_events(
            lung_cancer_codes, on_or_before="index_date - 1 day"
        ),
    ),
    cov_cat_haem_cancer=patients.with_these_clinical_events(
        haem_cancer_codes, on_or_before="index_date - 1 day"
    ),
    cov_cat_asthma=patients.satisfying(
        """
            recent_asthma_code OR (
              asthma_code_ever AND NOT
              copd_code_ever
            )
        """,
        return_expectations={
            "incidence": 0.05,
        },
        recent_asthma_code=patients.with_these_clinical_events(
            asthma_codes,
            between=["index_date - 3 years", "index_date - 1 day"],
        ),
        asthma_code_ever=patients.with_these_clinical_events(asthma_codes),
        copd_code_ever=patients.with_these_clinical_events(
            chronic_respiratory_disease_codes
        ),
    ),
    ## Chronic obstructive pulmonary disease
    ### Primary care
    tmp_cov_bin_chronic_obstructive_pulmonary_disease_snomed=patients.with_these_clinical_events(
        copd_snomed_clinical,
        returning='binary_flag',
        on_or_before="index_date - 1 day",
        return_expectations={"incidence": 0.1},
    ),
    ### HES APC
    tmp_cov_bin_chronic_obstructive_pulmonary_disease_hes=patients.admitted_to_hospital(
        returning='binary_flag',
        with_these_diagnoses= copd_icd10,
        on_or_before="index_date - 1 day",
        return_expectations={"incidence": 0.1},
    ),
    ### Combined
    cov_cat_chronic_obstructive_pulmonary_disease=patients.maximum_of(
        "tmp_cov_bin_chronic_obstructive_pulmonary_disease_snomed", "tmp_cov_bin_chronic_obstructive_pulmonary_disease_hes",
    ),
    cov_cat_chronic_respiratory_disease=patients.with_these_clinical_events(
        chronic_respiratory_disease_codes, on_or_before="index_date - 1 day"
    ),
    cov_cat_chronic_cardiac_disease=patients.with_these_clinical_events(
        chronic_cardiac_disease_codes, on_or_before="index_date - 1 day"
    ),
    cov_cat_chronic_liver_disease=patients.with_these_clinical_events(
        chronic_liver_disease_codes, on_or_before="index_date - 1 day"
    ),
    cov_cat_dementia=patients.satisfying(
        "dementia",
        dementia=patients.with_these_clinical_events(
        dementia_codes, on_or_before="index_date - 1 day"
        ),
    ),
    cov_cat_stroke=patients.satisfying(
        "stroke",
        stroke=patients.with_these_clinical_events(
            stroke_gp_codes, on_or_before="index_date - 1 day"
        ),
    ),
    cov_cat_other_neuro=patients.with_these_clinical_events(
        other_neuro_codes, on_or_before="index_date - 1 day"
    ),
    cov_cat_organ_transplant=patients.with_these_clinical_events(
        organ_transplant_codes, on_or_before="index_date - 1 day"
    ),
    cov_cat_dysplenia=patients.with_these_clinical_events(
        spleen_codes, on_or_before="index_date - 1 day"
    ),
    cov_cat_ra_sle_psoriasis=patients.with_these_clinical_events(
        ra_sle_psoriasis_codes, on_or_before="index_date - 1 day"
    ),
    cov_cat_other_immunosuppressive_condition=patients.satisfying(
        """
           sickle_cell
        OR aplastic_anaemia
        OR hiv
        OR permanent_immunodeficiency
        OR temporary_immunodeficiency
        """,
        sickle_cell=patients.with_these_clinical_events(
            sickle_cell_codes, on_or_before="index_date - 1 day"
        ),
        aplastic_anaemia=patients.with_these_clinical_events(
            aplastic_codes, on_or_before="index_date - 1 day"
        ),
        hiv=patients.with_these_clinical_events(
            hiv_codes, on_or_before="index_date - 1 day"
        ),
        permanent_immunodeficiency=patients.with_these_clinical_events(
            permanent_immune_codes, on_or_before="index_date - 1 day"
        ),
        temporary_immunodeficiency=patients.with_these_clinical_events(
            temp_immune_codes, on_or_before = "index_date - 1 day"
        ),
    ),
    cov_cat_heart_failure=patients.with_these_clinical_events(
        heart_failure_codes, on_or_before = "index_date - 1 day"
    ),
    cov_cat_hypertension=patients.with_these_clinical_events(
        hypertension_codes, on_or_before = "index_date - 1 day"
    ),

    cov_cat_mental_health=patients.with_these_clinical_events(
        combine_codelists(psychosis_schizophrenia_bipolar_codes, depression_codes),
        on_or_before="index_date - 1 day",
    ),
    cov_cat_rheumatoid_arthritis=patients.with_these_clinical_events(
        rheumatoid_arthritis_codes, on_or_before = "index_date - 1 day"
    ),
    cov_cat_chronic_kidney_disease=patients.with_these_clinical_events(
        chronic_kidney_disease_codes, on_or_before = "index_date - 1 day"
    ),
    cov_cat_sle=patients.with_these_clinical_events(
        sle_codes, on_or_before = "index_date - 1 day"
    ),
    cov_cat_psoriasis=patients.with_these_clinical_events(
        psoriasis_codes, on_or_before = "index_date - 1 day"
    ),
)