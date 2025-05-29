agregar_serie = function(dados, unidade = "mensal"){
    serie_agrupada = 
    if (unidade == "mensal"){
      dados %>%
      mutate(data = yearmonth(as.Date(floor_date(ORDERDATE, unit = "month")))) %>%
      group_by(data) %>%
      summarise(vendas_totais = sum(SALES)) %>%
      as_tsibble()
    } else if (unidade == "diario"){
      dados %>%
      mutate(data = as.Date(floor_date(ORDERDATE, unit = "day"))) %>%
      group_by(data) %>%
      summarise(vendas_totais = sum(SALES)) %>%
      as_tsibble()
    } else if (unidade == "semanal"){
      dados %>%
      mutate(data = yearweek(as.Date(floor_date(ORDERDATE, unit = "week")))) %>%
      group_by(data) %>%
      summarise(vendas_totais = sum(SALES)) %>%
      as_tsibble()
    } else if (unidade == "trimestral"){
      dados %>%
      mutate(data = yearquarter(as.Date(floor_date(ORDERDATE, unit = "quarter")))) %>%
      group_by(data) %>%
      summarise(vendas_totais = sum(SALES)) %>%
      as_tsibble()
    } 
    
    return(serie_agrupada)
}

gerar_forecast = function(dados, periodos = 12){
  forecast = dados %>%
    model(TSLM(vendas_totais ~ data + trend() + season())) %>%
    forecast(h = periodos) %>%
    as_tibble() %>%
    select(forecast =  .mean, data) %>%
    full_join(dados) %>%
    pivot_longer(cols = -data,
                 names_to = "names",
                 values_to = "values")
  
  forecast = forecast %>%
    mutate(names = case_when(
    str_detect(names, "forecast") ~ "Forecast",
    str_detect(names, "vendas") ~ "Vendas"
  )) %>%
    mutate(texto = str_glue("{names}
                          Data: {data}
                          Vendas: {scales::dollar(values)}"))
  return(forecast)
}

grafico_forecast = function(dados){
  ggplotly(
    dados %>% ggplot(aes( x = data, y = values, color = names)) +
      geom_line() +
      geom_point(aes(text = texto), size = 0.1)+
      scale_y_continuous(labels = scales::dollar_format())+
      labs(x = NULL, y = NULL, color = NULL) +
      theme(legend.position = "none",
            plot.background = element_rect(fill = "black"),
            panel.background = element_rect(fill = "black"))+
      scale_color_manual(values = c("#375A7F", "#00bc8c")), 
    tooltip = "text"
  )
}

grafico_serie_temporal = function(dados){
  dados = dados %>%
    mutate(texto = str_glue("Vendas
                          Data: {data}
                          Vendas: {scales::dollar(vendas_totais)}"))
  
  ggplotly(
    dados %>%  ggplot(aes( x = data, y = vendas_totais)) +
      geom_line(color = "#00bc8c") +
      geom_point(aes(text = texto), size = 0.1, color = "#00bc8c")+
      scale_y_continuous(labels = scales::dollar_format())+
      labs(x = NULL, y = NULL, color = NULL) +
      theme(legend.position = "none",
            plot.background = element_rect(fill = "black"),
            panel.background = element_rect(fill = "black")),
    tooltip = "text"
  )
}

agrupar_por_pais = function(dados){
  dados %>% 
    group_by(COUNTRY) %>%
    summarise(vendas_totais_por_pais = sum(SALES)) %>%
    ungroup() %>%
    mutate(texto = str_glue("<b>País</b>: {COUNTRY} <br>
                            <b>Vendas</b>: {scales::dollar(vendas_totais_por_pais)}"))
}

mapa_pais = function(dados){
  #contorno dos países
  mapa_paises = worldgeojson
  
  #dados agrupados
  dados_agrupados = agrupar_por_pais(dados)
  
  # Definir a sequência de cores verdes em função dos valores das vendas
  cores_verdes = colorRampPalette(colors = c("#8FBC8F", "#006400"))(nrow(dados_agrupados))
  
  highchart() %>%
    hc_add_series_map(mapa_paises, dados_agrupados, value = "vendas_totais_por_pais", joinBy = c("name", "COUNTRY")) %>%
    hc_tooltip(
      useHTML = TRUE,
      headerFormat = "",
      pointFormat = "{point.texto}"
    ) %>%
    hc_colorAxis(stops = color_stops(nrow(dados_agrupados), cores_verdes))
}
 






