# YW added clinical variables from AW, added heart failure, smoking status, age>=18 years
# YW adapted from AW, 10 November 2021

# Long COVID risk factors and prediction models

# --- IMPORT STATEMENTS ---
from cohortextractor import (
    StudyDefinition,
    Measure,
    patients,
    codelist,
    filter_codes_by_category,
    combine_codelists,
    codelist_from_csv,
)

# set seed so that dummy data can be reproduced
import numpy as np
np.random.seed(4366)

## Import codelists from codelist.py (which pulls them from the codelist folder)
from codelists import *
#from common_variables import demographic_variables, clinical_variables
from common_variables_dynamic import generate_common_variables
(
    demographic_variables,
    clinical_variables
) = generate_common_variables(index_date_variable = "index_date - 1 day", index_date_variable_3y = "index_date - 3 years")

pandemic_start = "2020-01-29"

def make_variable(code):
    return {
        f"snomed_{code}": (
            patients.with_these_clinical_events(
                codelist([code], system="snomed"),
                on_or_after=pandemic_start,
                returning="number_of_matches_in_period",
                include_date_of_match=True,
                date_format="YYYY-MM-DD",
                return_expectations={
                    "incidence": 0.1,
                    "int": {"distribution": "normal", "mean": 3, "stddev": 1},
                },
            )
        )
    }


def loop_over_codes(code_list):
    variables = {}
    for code in code_list:
        variables.update(make_variable(code))
    return variables


study = StudyDefinition(
    default_expectations={
        "date": {"earliest": "index_date", "latest": "today"},
        "rate": "uniform",
        "incidence": 0.05,
        "int": {"distribution": "normal", "mean": 25, "stddev": 5},
        "float": {"distribution": "normal", "mean": 25, "stddev": 5},
    },
    index_date="2020-12-01",
    population=patients.satisfying(
        "registered AND (cov_cat_sex = 'M' OR cov_cat_sex = 'F') AND cov_num_age >= 18 AND cov_num_age <= 105",
        registered=patients.registered_as_of("index_date"),
    ),
    # COVID infection
    sgss_positive=patients.with_test_result_in_sgss(
        pathogen="SARS-CoV-2",
        test_result="positive",
        returning="date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
       # return_expectations={"incidence": 0.1, "date": {"earliest": "index_date"}}, 
       return_expectations={"incidence": 0.1, "date": {"earliest": pandemic_start}}, 
    ),
    primary_care_covid=patients.with_these_clinical_events(
        any_primary_care_code,
        returning="date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        # return_expectations={"incidence": 0.1, "date": {"earliest": "index_date"}},
        return_expectations={"incidence": 0.1, "date": {"earliest": pandemic_start}},
    ),
    hospital_covid=patients.admitted_to_hospital(
        with_these_diagnoses=covid_codes,
        returning="date_admitted",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        #return_expectations={"incidence": 0.1, "date": {"earliest": "index_date"}},
        return_expectations={"incidence": 0.1, "date": {"earliest": pandemic_start}},
    ),
    # Outcome
    out_covid_date = patients.minimum_of(
        "sgss_positive", "primary_care_covid", "hospital_covid"
    ),
    out_long_covid=patients.with_these_clinical_events(
        any_long_covid_code,
        return_expectations={"incidence": 0.05},
    ),
    out_first_long_covid_date=patients.with_these_clinical_events(
        any_long_covid_code,
        returning="date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={"incidence": 0.1, "date": {"earliest": "index_date"}},
    ),
    #"loop_over_codes" provides any long covid code that a patient has been given and the first time that the specific code is given.
    # because of this, the first long covid date can also be derived outside study definition
    **loop_over_codes(any_long_covid_code),
    out_first_long_covid_code=patients.with_these_clinical_events(
        # when running on real data, any long covid code will be returned 
        any_long_covid_code,
        returning="code",
        find_first_match_in_period=True,
        return_expectations={
            "incidence": 0.05,
            "category": {
                "ratios": {
                    #Post-COVID-19 syndrome
                    "1325161000000102": 0.2,
                    #Ongoing symptomatic COVID-19
                    "1325181000000106": 0.2,
                    #Signposting to Your COVID Recovery
                    "1325021000000106": 0.3,
                    #Newcastle post-COVID syndrome Follow-up Screening Questionnaire
                    "1325051000000101": 0.2,
                    #Assessment using Newcastle post-COVID syndrome Follow-up Screening Questionnaire
                    "1325061000000103": 0.1,
                }
            },
        },
    ),
     #"loop_over_codes" provides any post viral fatigue code that a patient has been given and the first time that the specific code is given.
     # because of this, the post viral fatigue date can also be derived outside study definition
    cov_cat_post_viral_fatigue=patients.with_these_clinical_events(
        post_viral_fatigue_codes,
        on_or_after=pandemic_start,
        return_expectations={"incidence": 0.05},
    ),
    first_post_viral_fatigue_date=patients.with_these_clinical_events(
        post_viral_fatigue_codes,
        on_or_after=pandemic_start,
        returning="date",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={"incidence": 0.1, "date": {"earliest": "index_date"}},
    ),
    **loop_over_codes(post_viral_fatigue_codes),
    practice_id=patients.registered_practice_as_of(
        "index_date",
        returning="pseudo_id",
        return_expectations={
            "int": {"distribution": "normal", "mean": 1000, "stddev": 100},
            "incidence": 1,
        },
    ),
    
    #Death date selecting min date from primary care and ONS data
    death_date=patients.minimum_of(
        "primary_care_death_date", "ons_died_from_any_cause_date",
    # Define primary_care_death_date and ons_died_from_any_cause_date here to reduce clutter, as we do not need to distinguish them
    # Death date (primary care)
    primary_care_death_date=patients.with_death_recorded_in_primary_care(
        on_or_after="index_date",
        returning="date_of_death",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "exponential_increase",
        },
    ),
    #Death date (ONS)
    ons_died_from_any_cause_date=patients.died_from_any_cause(
        on_or_after="index_date",
        returning="date_of_death",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "index_date", "latest" : "today"},
            "rate": "exponential_increase",
        },
    ),
    ),
      ###  COVID vaccination
    # First covid vaccination date (first vaccine given on 8/12/2020 in the UK)
    vax_covid_date1=patients.with_tpp_vaccination_record(
        # code for TPP only, when using patients.with_tpp_vaccination_record() function
        target_disease_matches="SARS-2 CORONAVIRUS",
        on_or_after="2020-12-07",
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2020-12-08", "latest": "today"},
            "incidence": 0.7
        },
    ),
    # Second covid vaccination date (first second dose reported on 29/12/2020 in the UK)
    vax_covid_date2=patients.with_tpp_vaccination_record(
        # code for TPP only, when using patients.with_tpp_vaccination_record() function
        target_disease_matches="SARS-2 CORONAVIRUS",
        on_or_after="vax_covid_date1 + 14 days",  # Allowing for less days between 2 vaccination dates
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2020-12-29", "latest": "today"},
            "incidence": 0.6
        },
    ),
     # Booster covid vaccination date (first booster vaccine reported on 16/09/2022 in the UK)
    vax_covid_date3=patients.with_tpp_vaccination_record(
        # code for TPP only, when using patients.with_tpp_vaccination_record() function
        target_disease_matches="SARS-2 CORONAVIRUS",
        on_or_after="vax_covid_date2 + 14 days",  # Allowing for the least days since the 2nd vaccine
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2021-09-16", "latest": "today"},
            "incidence": 0.6
        },
    ),

    # ###No. primary care consultation in year prior to index date
    # cov_num_gp_consultation=patients.with_gp_consultations(
    #     between=["index_date - 12 months", "index_date"],
    #     returning="number_of_matches_in_period",
    #     return_expectations={
    #         "int": {"distribution": "poisson", "mean": 5},
    #     },
    # ),
    # Smoking status
    cov_cat_smoking_status=patients.categorised_as(
        {
            "S": "most_recent_smoking_code = 'S'",
            "E": """
                 most_recent_smoking_code = 'E' OR (
                   most_recent_smoking_code = 'N' AND ever_smoked
                 )
            """,
            "N": "most_recent_smoking_code = 'N' AND NOT ever_smoked",
            "M": "DEFAULT",
        },
        return_expectations={
            "category": {"ratios": {"S": 0.6, "E": 0.1, "N": 0.2, "M": 0.1}}
        },
        most_recent_smoking_code=patients.with_these_clinical_events(
            smoking_codes,
            find_last_match_in_period=True,
            on_or_before="index_date",
            returning="category",
        ),
        ever_smoked=patients.with_these_clinical_events(
            filter_codes_by_category(smoking_codes, include=["S", "E"]),
            on_or_before="index_date",
        ),
    ),

    **demographic_variables,
    **clinical_variables,
)
