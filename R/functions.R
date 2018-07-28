pasar_minutos <- function(tiempo){
  round(tiempo/60,1)
}

formatear_tiempo <- function(tiempo){
    horas <- tiempo %/% 3600
    minutos <-  (tiempo - horas * 3600) %/% 60
    segundos <- round(tiempo - horas * 3600 - minutos * 60)
    paste0(
        formatC(horas, width = 2, flag = "0"), "h ",
        formatC(minutos, width = 2, flag = "0"), "m ",
        formatC(segundos, width = 2, flag = "0"), "s."
    )
}

## debemos completar las trayectorias de un alumno, para que tengan todos valores de objetivos
## en los mismos instantes de tiempo
completar_objetivos <- function(objetivos.df, porcentajes){
    ## devuelve un df con porcentaje y tiempo, completa objetivos.df en los valores de porcentajes
    if (sum(c("tiempo", "porcentaje") %in% names(objetivos.df)) != 2){
        stop("objetivos.df no contiene columnas tiempo y porcentaje")
    }
    if (sum(is.na(objetivos.df$porcentaje)) > 0){
        stop("porcentaje contains NA")
    }
    objetivos.df$cut_porcentaje = cut(
        objetivos.df$porcentaje,
        c(-1, unique(objetivos.df$porcentaje)),
        right = TRUE,
        labels = FALSE
    )
    tibble::tibble(
        porcentaje = porcentajes,
        cut_porcentaje = cut(
            porcentajes,
            c(-1, unique(objetivos.df$porcentaje)),
            right = TRUE,
            labels = FALSE
        )
    ) %>%
        left_join(objetivos.df[c("tiempo", "cut_porcentaje")]) %>%
        select(tiempo, porcentaje)
}
## ----------------------------------------------------------------------------
##
## Función a aplicar a cada trozo de dataframe
##
## -----------------------------------------------------------------------------
## Prepara una lista que podamos pasar a hc_add_series_list según formato de la
## API de highcharts
colores_q <- setNames(
    c("black", "blue", "green"),
    c("Q1", "Mediana", "Q3")
)
prepara_trozo <- function(df){
##    browser()
    list(
        data = purrr::pmap(
            list(df$valor, df$porcentaje, df$n_q, df$variable),
            function(a, b, c, d) list(
                                     x = a,
                                     y = b,
                                     n = c,
                                     name = d,
                                     marker = list(enabled = FALSE)
                                 )
        ),
        name = df$variable[1L],
        color = setNames(colores_q[df$variable[1L]], NULL),
        step = "left"
    )
}

repartir_duracion <- function(fecha, duracion){
    dias <- seq(
        from = lubridate::floor_date(fecha, unit = "day"),
        to = lubridate::floor_date(fecha + duracion, unit = "day"),
        by = "day"
    )
    fechas_auxiliares <- c(
        fecha,
        seq(
            from = lubridate::ceiling_date(fecha, unit = "day"),
            by = "day",
            length.out = (length(dias) - 1)
        ),
        fecha + duracion
    )
    tibble::tibble(dia = dias, duracion = diff(fechas_auxiliares))
}
##

## prepara json_data
#' Manipula json de eventos y estructura del curso
#'
#'@import dplyr
#'
#' @param json_data A string
#'
#' @return una lista con los objetos necesarios para el cuadro de mandos shiny
#' @export
#'
#' @examples
#' preparar_json({{"a":3,"nombre":"Mathieu"} })
preparar_json <- function(json_data){
    json_objetos <- jsonlite::fromJSON(json_data)
    tipos <- unique(json_objetos$tipo)
    tipos_ordenados <- tibble::tibble(
                                   tipo = c(
                                       "LoggedIn",
                                       tipos[tipos != "LoggedIn"]
                                   ),
                                   tipo_id = 1:length(tipos)
                               )
    json_data <- json_objetos %>%
        mutate(
            porcentaje = as.numeric(porcentaje),
            unidad = formatC(
                as.integer(unidad),
                width = ceiling(log(max(as.integer(unidad)), 10)),
                format = "d",
                flag = "0"
            )
        ) %>%
        left_join(tipos_ordenados)

    ## ---------------------------------------------------------------------
    ## Preparamos un framework que identifique unidades con tu título actual
    ##
    ## si eso me lo paso Dani como metadatos, no habrá necesidad.
    ##
    unidades <- json_data %>%
        select(url, unidad, titulo) %>%
        distinct %>%
        group_by(url) %>%
        mutate(titulo_actual = tail(titulo, 1L)) %>%
        select(-titulo) %>%
        distinct %>%
        ungroup(.)


    ## esta parte se pasará por json
    usuarios <- json_data %>%
        select(usuario, correo, nombre, apellidos) %>%
        distinct

    ## ---------------------------------------------------------------------

    ## Para definir la sesion, usamos que hay siempre un LoggedIn
    ## al inicio de una sesión. Asignamos a la sesión todo lo que hay
    ## hasta el siguiente LoggedIn
    eventos <- json_data %>%
        mutate(fecha = lubridate::mdy_hms(fecha)) %>%
        arrange(fecha, tipo_id) %>%
        group_by(url) %>%
        group_by(usuario) %>%
        mutate(sesion = cumsum(tipo == "LoggedIn")) %>%
        ungroup()
    eventos <- eventos %>%
        filter(perfil == "alumno")
    ## obtenemos para cada evento en sesion, unidad y url, la duración hasta el evento,
    ## es decir, el tiempo transcurrido desde el inicio de la sesión hasta este
    ## evento y la duración acumulada previa el tiempo dedicado a la unidad, url
    ## antes de esta sesión para este usuario.
    eventos <- eventos  %>%
        group_by(usuario, sesion, url) %>%
        mutate(
            duracion_hasta_evento = as.numeric(fecha) - as.numeric(fecha[1L])
        )
    duracion_sesiones <- eventos  %>%
        group_by(usuario, sesion,  url) %>%
        summarise(
            duracion_sesion = diff(as.numeric(range(fecha)))
        ) %>%
        group_by(usuario, url) %>%
        mutate(
            duracion_acumulada_previa = cumsum(
                c(0, duracion_sesion)[1:length(duracion_sesion)]
            )
        )
    ## añadimos a eventos, la duración_acumulada previa (tiempo dedicada
    ## a la unidad en sesiones anteriores
    eventos <- eventos %>%
        left_join(duracion_sesiones)
    ## para cada evento, calculamos el tiempo empleado en la unidad para llegar
    ## hasta allí.
    eventos <- eventos %>%
        mutate(tiempo_empleado = duracion_hasta_evento +
                   duracion_acumulada_previa)


    numero_unidades_visitadas <- nrow(
        eventos %>%
        ungroup() %>%
        select(url, unidad) %>%
        distinct(.)
    )

    numero_alumnos <- nrow(
        eventos %>%
        ungroup() %>%
        select(usuario) %>%
        distinct(.)
    )

    ## -------------------------------------------------
    ## tiempo medio dedicado a cada unidad, url: calculamos por cada usuario
    ## el máximo del tiempo_empleado,
    tiempo_usuario <- eventos %>%
        group_by(usuario, url) %>%
        summarise(
            maxporcentaje = max(porcentaje),
            tiempo_unidad = max(tiempo_empleado)
        ) %>%
        ungroup()

    ## visitantes de cada unidad, url calculando también para los usuarios
    ## que han superado objetivo 100%
    unidades_visitadas_superadas <- tiempo_usuario %>%
        group_by(url) %>%
        summarise(
            visitantes = n_distinct(usuario),
            visitantes_100 = n_distinct(
                usuario[maxporcentaje >= 100]
            )
        ) %>%
        left_join(unidades) %>%
        arrange(as.integer(unidad), titulo_actual)

    ## tiempos y número de unidades visitadas superadas por cada alumno
    tiempos_usuarios <- tiempo_usuario %>%
        group_by(usuario) %>%
        summarise(
            visitadas = sum(tiempo_unidad),
            superadas = sum(tiempo_unidad[maxporcentaje >= 100])
        ) %>%
        tidyr::gather(
                   key = "tipo",
                   value = "tiempo",
                   visitadas, superadas
               )
    unidades_usuarios <- tiempo_usuario %>%
        group_by(usuario) %>%
        summarise(
            visitadas = n_distinct(url),
            superadas = sum(maxporcentaje >= 100)
        ) %>%
        tidyr::gather(
                   key = "tipo",
                   value = "unidades",
                   visitadas, superadas
               )

    tiempos_unidades <- tiempos_usuarios %>%
        left_join(unidades_usuarios) %>%
        left_join(usuarios) %>%
        mutate(nombre_apellidos = paste(nombre, apellidos))



    lookup <- c("usuario", "nombre", "apellidos", unidades$titulo_actual) %>%
        setNames(c("usuario", "nombre", "apellidos", unidades$url))
    tiempo_usuario_tp <- tiempo_usuario %>%
        left_join(unidades) %>%
        ungroup()
    tiempo_usuario_t <- tiempo_usuario_tp %>%
        left_join(usuarios) %>%
        select(correo, nombre, apellidos, url, tiempo_unidad) %>%
        mutate(tiempo_unidad = formatear_tiempo(tiempo_unidad)) %>%
        tidyr::spread(key = url, value = tiempo_unidad)
    names(tiempo_usuario_t) <- lookup[names(tiempo_usuario_t)]
    tiempo_usuario_p <- tiempo_usuario_tp %>%
        left_join(usuarios) %>%
        select(usuario, nombre, apellidos, url, maxporcentaje) %>%
        tidyr::spread(key = url, value = maxporcentaje)
    names(tiempo_usuario_p) <- lookup[names(tiempo_usuario_p)]
    ## ----------------------------------------------------------------------------
    ##
    ## Preparamos los datos para cuartiles de consecución de objetivos
    ##
    ## -----------------------------------------------------------------------------
    objetivos <- eventos %>%
        group_by(usuario, url)  %>%
        arrange(porcentaje) %>%
        filter(!duplicated(porcentaje))
    porcentajes_unidad <- objetivos %>%
        group_by(url) %>%
        summarise(porcentajes = list(sort(unique(porcentaje))))
    ## completamos los datos de cada usuario, para que todos tengan datos
    ## en todos los porcentajes de la unidad, de manera que se pueda calcular
    ## cuartiles (se debe a que pueden cumplir objetivos de manera desordenada
    ## lo que puede llevar a que unos tengan valores de porcentajes que otros no tengan
    ## eso es raro, visto lo que me ha dicho Dani que los porcentajes son 100/ numero de items
    ## por conseguir en la unidad.
    objetivos_completados <- objetivos %>%
        group_by(usuario,  url) %>%
        select(tiempo = tiempo_empleado, porcentaje) %>%
        tidyr::nest() %>%
        left_join(porcentajes_unidad) %>%
        mutate(
            porcentajes_completados = purrr::map2(
                                                 data,
                                                 porcentajes,
                                                 ~ completar_objetivos(objetivos.df = .x, porcentajes = .y)
                                             )
        ) %>%
        select(- data, - porcentajes) %>%
        tidyr::unnest()

    alcance_cuartiles <-  objetivos_completados %>%
        mutate(tiempo = pasar_minutos(tiempo)) %>%
        group_by(url, porcentaje) %>%
        summarise(
            Q1 = quantile(tiempo, 0.25, na.rm = TRUE),
            Mediana = quantile(tiempo, 0.5, na.rm = TRUE),
            Q3 = quantile(tiempo, 0.75, na.rm = TRUE),
            n_q = sum(!is.na(tiempo))
        )

    alcance_cuartiles.long <- alcance_cuartiles %>%
        tidyr::gather(key = "variable", value = "valor", Q1:Q3 ) %>%
        group_by(url, variable) %>%
        mutate(
            trozo = c(TRUE, diff(valor) < 0),
            trozo = cumsum(trozo)
        )
    ## l_df <- split(
    ##         alcance_cuartiles.long,
    ##         list(
    ##             alcance_cuartiles.long$url,
    ##             alcance_cuartiles.long$variable,
    ##             alcance_cuartiles.long$trozo)
    ## )
    alcance_cuartiles_df <- alcance_cuartiles.long %>%
        mutate(variable2 = variable) %>%
        group_by(url, trozo, variable2) %>%
        tidyr::nest()

    trozos <- purrr::map(alcance_cuartiles_df$data, ~ prepara_trozo(.x))
    lista_trozos <- list(trozos = trozos, url = alcance_cuartiles_df$url)


    ## ------------------------------------------------------------------------------------------
    ##
    ##  Dedicación diaria de cada alumno
    ## Se dedica buena parte del código al reparto de duración de sesiones que
    ## empiezan un día pero pasan la media noche y deben imputarse al día siguiente
    ## ------------------------------------------------------------------------------------------

    logins <- eventos %>%
        filter(tipo == "LoggedIn")
    ## tenemos que repartir la duración sobre días y una sesion empieza un día
    ## acaba en otro.
    ## empezamos por no hacer nada en los logins donde no hay necesidad de
    ## repartir tiempo
    logins <- logins %>%
        mutate(
            necesita_reparto = lubridate::floor_date(fecha, unit = "day") !=
                lubridate::floor_date(fecha + duracion_sesion, unit = "day")
        )
    dedicacion_diaria_vacio <- tibble::tibble(
                                           usuario = character(),
                                           fecha = as.Date(character()),
                                           sesion = integer(),
                                           duracion_sesion = numeric(),
                                           dia = as.Date(character()),
                                           duracion = as.difftime(numeric(), units = "secs")
                                       )
    if (sum(!logins$necesita_reparto) > 0){
        dedicacion_diaria_sin_reparto <- logins %>%
            filter(!necesita_reparto) %>%
            ungroup() %>%
            select(
                usuario,
                fecha,
                sesion,
                duracion_sesion
            ) %>%
            mutate(
                dia = lubridate::floor_date(fecha, unit = "day"),
                duracion = as.difftime(duracion_sesion, units = "secs")
            )
    } else {
        dedicacion_diaria_sin_reparto <- dedicacion_diaria_vacio
    }

    if(sum(logins$necesita_reparto) > 0){
        logins_con_reparto <- logins %>%
            filter(necesita_reparto) %>%
            mutate(
                df_duracion = purrr::map2(
                                         fecha,
                                         duracion_sesion,
                                         ~ repartir_duracion(.x, .y)
                                     )
            )
        ##Cogemos el Loggin de cada sesión, le calculamos su duración y se la
        ## asignamos a ese día
        dedicacion_diaria_con_reparto <- logins_con_reparto %>%
            ungroup() %>%
            select(
                usuario,
                fecha,
                sesion,
                duracion_sesion,
                df_duracion
            ) %>%
            tidyr::unnest()
        ## nos aseguramos que estamos trabajando en segundos
        units(dedicacion_diaria_con_reparto$duracion) <- "secs"
    } else {
        dedicacion_diaria_con_reparto <- dedicacion_diaria_vacio
    }
    dedicacion_diaria <-
        rbind(
            dedicacion_diaria_sin_reparto,
            dedicacion_diaria_con_reparto
        ) %>%
        group_by(usuario, dia) %>%
        summarise(
            tiempo_dedicado = as.numeric(sum(duracion))
        ) %>%
        left_join(usuarios)


    ## ------------------------------------------------------------------------------------------
    ##
    ##  Lo recogemos todo en una lista para pasarlo en json
    ##
    ## ------------------------------------------------------------------------------------------

    datos <- list(
        numero_unidades_visitadas= numero_unidades_visitadas,
        numero_alumnos = numero_alumnos,
        unidades = unidades,
        usuarios = usuarios,
        unidades_visitadas_superadas = unidades_visitadas_superadas,
        tiempos_unidades = tiempos_unidades,
        tiempo_usuario_t = tiempo_usuario_t,
        tiempo_usuario_p = tiempo_usuario_p,
        tiempo_usuario = tiempo_usuario,
        alcance_cuartiles = alcance_cuartiles,
        lista_trozos_cuartiles = lista_trozos,
        dedicacion_diaria = dedicacion_diaria
    )
    datos
}

## ------------------------------------------------------------------------------------------
##
##  La función de preparación de los datos para el cuadro de mando alumnos
##
## ------------------------------------------------------------------------------------------


## prepara json_data_alumno
#' Manipula json de eventos para un alumno
#'
#'@import dplyr
#'
#' @param json_data A string
#'
#' @return una lista con los objetos necesarios para el cuadro de mandos shiny de alumno
#' @export
#'
#' @examples
#' preparar_json_alumno({{"a":3,"nombre":"Mathieu"} })
preparar_json_alumno<- function(json_data){
    json_data <- jsonlite::fromJSON(json_data)
    tipos <- unique(json_data$tipo)
    tipos_ordenados <- tibble(
        tipo = c(
            "LoggedIn",
            tipos[tipos != "LoggedIn"]
        ),
        tipo_id = 1:length(tipos)
    )
    json_data <- json_data %>%
        mutate(
            porcentaje = as.numeric(porcentaje),
            unidad = formatC(
                as.integer(unidad),
                width = ceiling(log(max(as.integer(unidad)), 10)),
                format = "d",
                flag = "0"
            )
        ) %>%
        left_join(tipos_ordenados)

    ## ---------------------------------------------------------------------

    ## Para definir la sesion, usamos que hay siempre un LoggedIn
    ## al inicio de una sesión. Asignamos a la sesión todo lo que hay
    ## hasta el siguiente LoggedIn
    eventos <- json_data %>%
        mutate(fecha = lubridate::mdy_hms(fecha)) %>%
        arrange(fecha, tipo_id) %>%
        group_by(unidad, url) %>%
        mutate(titulo_actual = tail(titulo, 1L)) %>%
        group_by(correo) %>%
        mutate(sesion = cumsum(tipo == "LoggedIn")) %>%
        ungroup()
    eventos <- eventos %>%
        filter(perfil == "alumno")
    ## obtenemos para cada evento en sesion, unidad y url, la duración hasta el evento,
    ## es decir, el tiempo transcurrido desde el inicio de la sesión hasta este
    ## evento y la duración acumulada previa el tiempo dedicado a la unidad, url
    ## antes de esta sesión para este usuario.
    eventos <- eventos  %>%
        group_by(correo, sesion, unidad, url) %>%
        mutate(
            duracion_hasta_evento = as.numeric(fecha) - as.numeric(fecha[1L])
        )
    duracion_sesiones <- eventos  %>%
        group_by(correo, sesion, unidad, url) %>%
        summarise(
            duracion_sesion = diff(as.numeric(range(fecha)))
        ) %>%
        ungroup() %>%
        group_by(correo, unidad, url) %>%
        mutate(
            duracion_acumulada_previa = cumsum(
                c(0, duracion_sesion)[1:length(duracion_sesion)]
            )
        )
    ## añadimos a eventos, la duración_acumulada previa (tiempo dedicada
    ## a la unidad en sesiones anteriores
    eventos <- eventos %>%
        left_join(duracion_sesiones)
    ## para cada evento, calculamos el tiempo empleado en la unidad para llegar
    ## hasta allí.
    eventos <- eventos %>%
        mutate(tiempo_empleado = duracion_hasta_evento +
                   duracion_acumulada_previa)

    ## -------------------------------------------------
    ## tiempo medio dedicado a cada unidad, url: calculamos por cada usuario
    ## el máximo del tiempo_empleado,
    tiempo_usuario <- eventos %>%
        group_by(correo, nombre, apellidos, unidad, url, titulo_actual) %>%
        summarise(
            maxporcentaje = max(porcentaje),
            tiempo_unidad = max(tiempo_empleado)
        ) %>%
        ungroup()


    ##
    ## --------------------------------------------------------------------------
    numero_unidades_visitadas <-  nrow(
        eventos %>%
        ungroup() %>%
        select(url, unidad) %>%
        distinct(.)
    )

    numero_unidades_superadas <- nrow(
        tiempo_usuario %>%
        ungroup() %>%
        filter(maxporcentaje >= 100) %>%
        select(unidad, url) %>%
        distinct(.)
    )

    tiempo_dedicado <- formatear_tiempo(sum(tiempo_usuario$tiempo_unidad))
    ## ------------------------------------------------------------------------------------------
    ##
    ##  Para dedicación diaria
    ##
    ## ------------------------------------------------------------------------------------------


    logins <- eventos %>%
        filter(tipo == "LoggedIn")
    ##Cogemos el Loggin de cada sesión, le calculamos su duración y se la asignamos a ese día
    ## tenemos que repartir la duración sobre días y una sesion empieza un día
    ## acaba en otro.
    ## empezamos por no hacer nada en los logins donde no hay necesidad de
    ## repartir tiempo
    logins <- logins %>%
        mutate(
            necesita_reparto = lubridate::floor_date(fecha, unit = "day") !=
                lubridate::floor_date(fecha + duracion_sesion, unit = "day")
        )
    dedicacion_diaria_vacio <- tibble(
        usuario = character(),
        correo = character(),
        nombre = character(),
        apellidos = character(),
        fecha = as.Date(character()),
        sesion = integer(),
        duracion_sesion = numeric(),
        dia = as.Date(character()),
        duracion = as.difftime(numeric(), units = "secs")
    )
    if (sum(!logins$necesita_reparto) > 0){
        dedicacion_diaria_sin_reparto <- logins %>%
            filter(!necesita_reparto) %>%
            ungroup() %>%
            select(
                usuario,
                correo,
                nombre,
                apellidos,
                fecha,
                sesion,
                duracion_sesion
            ) %>%
            mutate(
                dia =  lubridate::floor_date(fecha, unit = "day"),
                duracion = as.difftime(duracion_sesion, units = "secs")
            )
    } else {
        dedicacion_diaria_sin_reparto <- dedicacion_diaria_vacio
    }

    if(sum(logins$necesita_reparto) > 0){
        logins_con_reparto <- logins %>%
            filter(necesita_reparto) %>%
            mutate(
                df_duracion = purrr::map2(
                                         fecha,
                                         duracion_sesion,
                                         ~ repartir_duracion(.x, .y)
                                     )
            )
                                        #Cogemos el Loggin de cada sesión, le calculamos su duración y se la
        ## asignamos a ese día
        dedicacion_diaria_con_reparto <- logins_con_reparto %>%
            ungroup() %>%
            select(
                usuario,
                correo,
                nombre,
                apellidos,
                fecha,
                sesion,
                duracion_sesion,
                df_duracion
            ) %>%
            tidyr::unnest()
        ## nos aseguramos que estamos trabajando en segundos
        units(dedicacion_diaria_con_reparto$duracion) <- "secs"
    } else {
        dedicacion_diaria_con_reparto <- dedicacion_diaria_vacio
    }
    dedicacion_diaria <- rbind(
        dedicacion_diaria_sin_reparto,
        dedicacion_diaria_con_reparto
    ) %>%
        group_by(correo, nombre, apellidos, dia) %>%
        summarise(
            tiempo_dedicado = as.numeric(sum(duracion))
        )

    ## ------------------------------------------------------------------------------------------
    ##
    ##  Lo recogemos todo en una lista para pasarlo en json
    ##
    ## ------------------------------------------------------------------------------------------

    list(
        numero_unidades_visitadas = numero_unidades_visitadas,
        numero_unidades_superadas = numero_unidades_superadas,
        tiempo_dedicado = tiempo_dedicado,
        dedicacion_diaria = dedicacion_diaria,
        tiempo_usuario = tiempo_usuario
    )

}
