round_km <- function(data, time, event, strata, threshold=6){
  stopifnot("Missing values not allow in `time`" = all(!is.na(data[[time]])))
  stopifnot("Missing values not allow in `event`" = all(!is.na(data[[event]])))
  dat_surv <-
    data %>%
    select(all_of(c(time, event, strata))) %>%
    dplyr::rename(time={{ time }}, event = {{ event }}) %>%
    group_by(across(all_of(strata))) %>%
    nest() %>%
    mutate(
      n_events = map_int(data, ~sum(.x[[event]], na.rm=TRUE)),
      surv_obj = map(data, ~{
        survfit(Surv(time, event) ~ 1, data = .x, conf.type="log-log")
      }),
      surv_obj_tidy = map(surv_obj, ~broom::tidy(.x)),
    ) %>%
    dplyr::select(strata, n_events, surv_obj_tidy) %>%
    unnest(surv_obj_tidy)
  dat_surv_rounded <-
    dat_surv %>%
    mutate(
      # Use ceiling not round. This is slightly biased upwards,
      # but means there's no disclosure risk at the boundaries (0 and 1) where masking would otherwise be threshold/2
      surv = ceiling_any(estimate, 1/floor(max(n.risk, na.rm=TRUE)/(threshold))),
      surv.ll = ceiling_any(conf.low, 1/floor(max(n.risk, na.rm=TRUE)/(threshold))),
      surv.ul = ceiling_any(conf.high, 1/floor(max(n.risk, na.rm=TRUE)/(threshold))),
      cml.event = ceiling_any(cumsum(n.event), threshold),
      cml.censor = ceiling_any(cumsum(n.censor), threshold),
      n.event = c(NA, diff(cml.event)),
      n.censor = c(NA, diff(cml.censor)),
      n.risk = ceiling_any(max(n.risk, na.rm=TRUE), threshold) - (cml.event + cml.censor)
    ) %>%
    dplyr::select(all_of(strata), time, surv, surv.ll, surv.ul, n.risk, n.event, n.censor)
  dat_surv_rounded
}