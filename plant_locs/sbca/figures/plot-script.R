
# Packages
  library(pacman)
  p_load(
    tidyverse, maps, readxl, janitor,
    patchwork, viridis, extrafont, latex2exp,
    fst, data.table, lubridate,
    sf, plyr, viridis, ggsn,
    parallel, here, magrittr
  )
# Load data
  border_dist = read_fst(here("presentations","uo_microgroup_w21","figures","border-dist.fst"))
  gen_water = read_fst(here("presentations","uo_microgroup_w21","figures","gen-water.fst"))

# Plot colors
  pc = magma(5, begin = 0.1, end = 0.9)

# Plot: Distance from generating unit to county border by fuel type
  plot_county_1 = ggplot(
    # data = gen_border_dist,
    data = border_dist %>% 
      filter(fuel_plot == "Coal" | fuel_plot == "Gas") %>%
      filter(fuel_plot != "Uniform US Grid") ,
    aes(x = dist_county_plot, fill = fuel_plot )
  ) +
  geom_vline(
    xintercept = 0, 
    size = 1/4
  ) +
  geom_histogram(
    aes(y = ..density..),
    color = NA,
    bins = 50
  ) + 
  geom_hline(
    yintercept = 0, 
    size = 1/4
  ) +
  ggtitle(
    TeX(paste0(
      "\\textbf{Panel A:} Distance to nearest \\textbf{county} border"
    )),
    TeX("2018 operating/stand-by units, capacity $\\geq 25$ MW")
  ) +
  scale_x_continuous(
    TeX("Distance to nearest \\textbf{county} border (km)"),
    breaks = seq(0, 25, 5),
    labels = c(seq(0, 20, 5), TeX("$\\geq 25$")), 
  ) +
  scale_y_continuous(
    "Density"
  ) +
  scale_fill_manual(values = pc[1:2]) +
    # scale_fill_manual(values = pc[2:5]) +
    theme_minimal(base_size = 20, base_family = "LM Roman 10") +
    theme(legend.position = "none") +
    facet_grid(
      vars(fuel_plot)
    )
  
  # Plot: Distance from generating unit to county border by fuel type
  plot_county_2 = ggplot(
    # data = gen_border_dist,
    data = border_dist %>% 
      filter(fuel_plot == "Hydro." | fuel_plot == "Solar/Wind") %>%
      filter(fuel_plot != "Uniform US Grid") ,
    aes(x = dist_county_plot, fill = fuel_plot )
  ) +
    geom_vline(
      xintercept = 0, 
      size = 1/4
    ) +
    geom_histogram(
      aes(y = ..density..),
      color = NA,
      bins = 50
    ) + 
    geom_hline(
      yintercept = 0, 
      size = 1/4
    ) +
    ggtitle(
      TeX(paste0(
        "\\textbf{Panel A:} Distance to nearest \\textbf{county} border"
      )),
      TeX("2018 operating/stand-by units, capacity $\\geq 25$ MW")
    ) +
    scale_x_continuous(
      TeX("Distance to nearest \\textbf{county} border (km)"),
      breaks = seq(0, 25, 5),
      labels = c(seq(0, 20, 5), TeX("$\\geq 25$")), 
    ) +
    scale_y_continuous(
      "Density"
    ) +
    scale_fill_manual(values = pc[3:4]) +
    # scale_fill_manual(values = pc[2:5]) +
    theme_minimal(base_size = 20, base_family = "LM Roman 10") +
    theme(legend.position = "none") +
    facet_grid(
      vars(fuel_plot)
    )
  
  
  
  
  ggsave(
    here("presentations","sbca","final_figs","county_dist_1.png"), 
    plot_county_1,
    dpi = 300,
    height = 8,
    width = 12
  )
  
  ggsave(
    here("presentations","sbca","final_figs","county_dist_2.png"), 
    plot_county_2,
    dpi = 300,
    height = 8,
    width = 12
  )
# Plot: Distance from generating unit to state border by fuel type

  
  
  # Plot: Distance from generating unit to state border by fuel type
  plot_state_1 = ggplot(
    # data = gen_border_dist,
    data = border_dist %>%
      filter(fuel_plot != "Uniform US Grid") %>% 
      filter(fuel_plot == "Coal" | fuel_plot == "Gas"),
    aes(x = dist_state_plot, fill = fuel_plot)
  ) +
    geom_vline(
      xintercept = 0, 
      size = 1/4
    ) +
    geom_histogram(
      aes(y = ..density..),
      color = NA,
      bins = 50
    ) + 
    geom_hline(
      yintercept = 0, 
      size = 1/4
    ) +
    ggtitle(
      TeX(paste0(
        "\\textbf{Panel B:} Distance to nearest \\textbf{state} border"
      )),
      TeX("2018 operating/stand-by units, capacity $\\geq 25$ MW")
    ) +
    scale_x_continuous(
      TeX("Distance to nearest \\textbf{state} border (km)"),
      breaks = seq(0, 150, 50),
      labels = c(seq(0, 100, 50), TeX("$\\geq 150$")), 
    ) +
    scale_y_continuous(
      "Density"
    ) +
    scale_fill_manual(values = pc[1:2]) +
    # scale_fill_manual(values = pc[2:5]) +
    theme_minimal(base_size = 20, base_family = "LM Roman 10") +
    theme(legend.position = "none") +
    facet_grid(
      vars(fuel_plot)
    )
  
  plot_state_2 = ggplot(
    # data = gen_border_dist,
    data = border_dist %>% 
      filter(fuel_plot != "Uniform US Grid")%>% 
      filter(fuel_plot == "Hydro." | fuel_plot == "Solar/Wind"),
    aes(x = dist_state_plot, fill = fuel_plot)
  ) +
    geom_vline(
      xintercept = 0, 
      size = 1/4
    ) +
    geom_histogram(
      aes(y = ..density..),
      color = NA,
      bins = 50
    ) + 
    geom_hline(
      yintercept = 0, 
      size = 1/4
    ) +
    ggtitle(
      TeX(paste0(
        "\\textbf{Panel B:} Distance to nearest \\textbf{state} border"
      )),
      TeX("2018 operating/stand-by units, capacity $\\geq 25$ MW")
    ) +
    scale_x_continuous(
      TeX("Distance to nearest \\textbf{state} border (km)"),
      breaks = seq(0, 150, 50),
      labels = c(seq(0, 100, 50), TeX("$\\geq 150$")), 
    ) +
    scale_y_continuous(
      "Density"
    ) +
    scale_fill_manual(values = pc[3:4]) +
    # scale_fill_manual(values = pc[2:5]) +
    theme_minimal(base_size = 20, base_family = "LM Roman 10") +
    theme(legend.position = "none") +
    facet_grid(
      vars(fuel_plot)
    )
  
  
  ggsave(
    here("presentations","sbca","final_figs","state_dist_1.png"), 
    plot_state_1,
    dpi = 300,
    height = 8,
    width = 12
  )
  
  
  ggsave(
    here("presentations","sbca","final_figs","state_dist_2.png"), 
    plot_state_2,
    dpi = 300,
    height = 8,
    width = 12
  )
  
  
# Plot histogram of distance to water by fuel category
  plot_water = ggplot(
    data = gen_water%>% filter(fuel_plot != "Uniform US Grid"),
    aes(x = dist_water_plot, fill = fuel_plot)
  ) +
  geom_vline(
    xintercept = 0, 
    size = 1/4
  ) +
  geom_histogram(
    aes(y = ..density..),
    color = NA,
    bins = 50
  ) + 
  geom_hline(
    yintercept = 0, 
    size = 1/4
  ) +
  ggtitle(
    TeX(paste0(
      "\\textbf{Panel A:} Distance to nearest body of water"
    )),
    TeX("2018 operating/stand-by units, capacity $\\geq 25$ MW")
  ) +
  scale_x_continuous(
    TeX("Distance (m)"),
    breaks = seq(0, 1e3, 200),
    labels = c(seq(0, 800, 200), TeX("$\\geq 1,000$")), 
  ) +
  scale_y_continuous(
    "Density"
  ) +
  scale_fill_manual(values = pc[1:4]) +
  theme_minimal(base_size = 16, base_family = "LM Roman 10") +
  theme(legend.position = "none") +
  facet_wrap(vars(fuel_plot))
  
  
  ggsave(
    here("presentations","sbca","final_figs","water_dist.png"), 
    plot_water,
    dpi = 300,
    height = 8,
    width = 12
  )
  
  
  