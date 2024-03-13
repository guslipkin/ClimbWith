#' user_popups
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
# .make_contributor <- function(name, bio, website = NULL, image = NULL) {
#   if (!is.null(website)) {
#     name <-
#       shiny::tags$h4(
#         shiny::tags$a(
#           href = website,
#           name,
#           shiny::tags$sup(
#             htmltools::img(src = 'www/images/box-arrow-up-right.svg', width = '1.75%')
#           ),
#           target = '_blank',
#           style = 'color: inherit;'
#         )
#       )
#   }
# }

show_about_us <- function() {
  shinyWidgets::show_alert(
    title = 'About Us',
    text = shiny::tagList(
      shiny::fluidPage(
        shiny::tags$h2('ClimbWith'),
        shiny::tags$p(
          "Gus started ClimbWith because he wanted to see where local gyms were
          located and what features they offered, beyond what's usually
          advertised. ClimbWith aims to provide the most accurate information on
          the features each gym offers such as climbing type and training
          boards, information about fitness equipment, and stats about wall
          heights.",
          style = 'font-size: .8em; text-align: justify;'
        ),
        shiny::tags$br(),
        shiny::tags$h2('The Team'),
        shiny::tags$h4(
          .get_tag_link('https://guslipkin.me', 'Gus Lipkin')
        ),
        shiny::fluidRow(
          shiny::column(
            width = 4,
            htmltools::img(
              src = 'www/images/gus_first_lead_comp.jpg', width = '80%'
            )
          ),
          shiny::column(
            width = 8,
            shiny::tags$p(
              "Gus has been climbing since September 2023. He started out in the
              gym, first with bouldering, then top rope. In February 2024, Gus
              registered for a lead competition without any experience and spent
              the next three weeks frantically learning to lead. At the
              competition, he only completed one route and placed last in his
              category, but was happy to have participated. He occasionally
              posts videos of himself using tall person beta on his Instagram, ",
              .get_tag_link('https://www.instagram.com/gus_is_tall/', '@gus_is_tall'),
              ".",
              style = 'font-size: .7em; text-align: justify;'
            )
          )
        )
      )
    ),
    html = TRUE,
    btn_labels = 'Close'
  )
}
