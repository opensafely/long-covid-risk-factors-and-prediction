# Patient characteristics associated with clinically coded long COVID: an OpenSAFELY cohort study using linked electronic health records

This is the code and configuration for long-COVID-risk-factors.

You can run this project via [Gitpod](https://gitpod.io) in a web browser by clicking on this badge: [![Gitpod ready-to-code](https://img.shields.io/badge/Gitpod-ready--to--code-908a85?logo=gitpod)](https://gitpod.io/#https://github.com/opensafely/long-COVID-prediction)

* The paper is [here](https://www.medrxiv.org/content/10.1101/2023.06.23.23291776v1)
* The study protocol is [here](https://github.com/opensafely/long-covid-risk-factors-and-prediction/blob/main/protocol/Wei%20-%20protocol-long%20covid%20risk%20factors.pdf)
* Raw model outputs, including charts, crosstabs, etc, are in `released_outputs/`
* If you are interested in how we defined our variables, take a look at the study definitions for [primary and pre-vaccination cohorts](analysis/study_definition_all.py), [post-vaccination cohort](analysis/study_definition_vaccinated.py) and [post-COVID cohort](analysis/study_definition_infected.py); these are written in `python`.
* If you are interested in how we defined our code lists, look in the [codelists folder](./codelists/).
* Developers and epidemiologists interested in the framework should review [the OpenSAFELY documentation](https://docs.opensafely.org)

# About the OpenSAFELY framework

The OpenSAFELY framework is a Trusted Research Environment (TRE) for electronic
health records research in the NHS, with a focus on public accountability and
research quality.

Read more at [OpenSAFELY.org](https://opensafely.org).

# Licences
As standard, research projects have a MIT license. 
