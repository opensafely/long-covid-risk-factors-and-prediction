# 01 June 2022
# background: some exploration on the possibility to create more refined categories for the following covariates:
# cancer(non-haematological), haematological cancer, asthma, diabetes and reduced kidney function


# pre_cov_num_creatinine=patients.with_these_clinical_events(
#     creatinine_codes,
#     find_last_match_in_period=True,
#     on_or_before="2020-02-01",
#     returning="numeric_value",
#     include_date_of_match=True,
#     include_month=True,
#     return_expectations={
#         "float": {"distribution": "normal", "mean": 60.0, "stddev": 15},
#         "incidence": 0.95,
#     },
# ),
# pre_cov_dialysis_date=patients.with_these_clinical_events(
#     dialysis_codes, return_first_date_in_period=True, include_month=True,
# ),

# pre_cov_hba1c_mmol_per_mol=patients.with_these_clinical_events(
#     hba1c_new_codes,
#     find_last_match_in_period=True,
#     on_or_before="2020-12-01",
#     returning="numeric_value",
#     include_date_of_match=True,
#     include_month=True,
#     return_expectations={
#         "float": {"distribution": "normal", "mean": 40.0, "stddev": 20},
#         "incidence": 0.95,
#     },
# ),
# pre_cov_hba1c_percentage=patients.with_these_clinical_events(
#     hba1c_old_codes,
#     find_last_match_in_period=True,
#     on_or_before="2020-12-01",
#     returning="numeric_value",
#     include_date_of_match=True,
#     include_month=True,
#     return_expectations={
#         "float": {"distribution": "normal", "mean": 5, "stddev": 2},
#         "incidence": 0.95,
#     },
# ),
# cov_cat_asthma=patients.satisfying(
#     """
#         recent_asthma_code OR (
#         asthma_code_ever AND NOT
#         copd_code_ever
#         )
#     """,
#     return_expectations={
#         "incidence": 0.05,
#    },
#     recent_asthma_code=patients.with_these_clinical_events(
#         asthma_codes,
#         # between=[f"{index_date_variable - 3 years}",f"{index_date_variable}"],
#         between=[f"{index_date_variable_3y}",f"{index_date_variable}"],
#     ),
#     asthma_code_ever=patients.with_these_clinical_events(asthma_codes),
#     copd_code_ever=patients.with_these_clinical_events(
#         chronic_respiratory_disease_codes
#     ),
# ),

# cov_cat_asthma=patients.categorised_as(
#   {
#     "0": "DEFAULT",
#     "1": """
#                     (
#                     recent_asthma_code OR (
#                         asthma_code_ever AND NOT
#                         copd_code_ever
#                     )
#                     ) AND (
#                     prednisolone_last_year = 0 OR 
#                     prednisolone_last_year > 4
#                     )
#                 """,
#     "2": """
#                     (
#                     recent_asthma_code OR (
#                         asthma_code_ever AND NOT
#                         copd_code_ever
#                     )
#                     ) AND
#                     prednisolone_last_year > 0 AND
#                     prednisolone_last_year < 5
#                     
#                 """,
#   },
#   return_expectations={"category": {"ratios": {"0": 0.8, "1": 0.1, "2": 0.1}},},
#   recent_asthma_code=patients.with_these_clinical_events(
#     #asthma_codes, between=["2017-12-01", "2020-12-01"],
#     asthma_codes, between=[f"{index_date_variable_3y}",f"{index_date_variable}"],
#   ),
#   asthma_code_ever=patients.with_these_clinical_events(asthma_codes),
#   copd_code_ever=patients.with_these_clinical_events(
#     chronic_respiratory_disease_codes
#   ),
#   prednisolone_last_year=patients.with_these_medications(
#     prednisolone_codes,
#     #between=["2019-12-01", "2020-12-01"],
#     between=[f"{index_date_variable_1y}",f"{index_date_variable}"],
#     returning="number_of_matches_in_period",
#   ),
# ), 
# pre_cov_diabetes_date=patients.with_these_clinical_events(
#     diabetes_codes, return_first_date_in_period=True, include_month=True,
# ),
# # https://github.com/ebmdatalab/tpp-sql-notebook/issues/32
# pre_cov_lung_cancer_date=patients.with_these_clinical_events(
#     lung_cancer_codes, return_first_date_in_period=True, include_month=True,
# ),
# pre_cov_haem_cancer_date=patients.with_these_clinical_events(
#     haem_cancer_codes, return_first_date_in_period=True, include_month=True,
# ),
# pre_cov_other_cancer_date=patients.with_these_clinical_events(
#     other_cancer_codes, return_first_date_in_period=True, include_month=True,
# ),