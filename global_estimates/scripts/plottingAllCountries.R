plottingTemporalVariationAllCountries <- function(data)
{
  data %>% 
    dplyr::group_by(country) %>%
    dplyr::filter(dplyr::n() > 20) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_at(dplyr::vars(country), dplyr::funs(factor(., levels=unique(.)))) %>%
    ggplot2::ggplot(ggplot2::aes(x = date)) +
    ggplot2::geom_line(ggplot2::aes(y = estimate)) + 
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = "dodgerblue", alpha = 0.3) + 
    ggplot2::facet_wrap(~country, scales = "free", ncol = 4) + 
    cowplot::theme_cowplot(font_size = 10)
  
}

underReportingData <- allHPCBayesianData()
plot <- underReportingData %>% plottingTemporalVariationAllCountries()

ggplot2::ggsave("~/tmp.png",
                plot,
                width = 10,
                height = 40,
                dpi = 300,
                limitsize = FALSE)


dataPlot <- underReportingData %>% 
  dplyr::filter(date == max(date)) %>% 
  dplyr::mutate(estimate = estimate,
                lower = lower,
                upper = upper) %>%
    dplyr::mutate(
      country = country  %>% 
        factor(levels = underReportingData %>% 
              dplyr::arrange(desc(estimate)) %>% 
                dplyr::pull(country) %>% 
                 unique()))


dataTable <- dataPlot %>%
  dplyr::mutate(estimate = paste0(signif(estimate,2),
                                  "%", "(", signif(lower,2), "%",
                                  "-", signif(upper, 2), "%", ")")) %>%
  dplyr::select(country, estimate, totalCases, totalDeaths)
