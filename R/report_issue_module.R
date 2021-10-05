

#' Report Issue Module UI
#'
#' @param id the module ID
#'
#' @export
#'
#' @importFrom shiny NS tags
#'
#' @return "Report Issue" button to go in the top bar of a shinydashboard Shiny app.
#'
report_issue_module_dash_ui <- function(id) {
  ns <- shiny::NS(id)

  tags$li(
    class = "dropdown",
    tags$a(
      id = ns("report_bug"),
      href = "#",
      onclick="return false;",
      tags$div(
        tags$i(
          class = "fa fa-bug"
        ),
        " Report Issue",
        style = "display: inline"
      )
    )
  )
}

#' Report Issue Module Server
#'
#' @param input the Shiny input
#' @param output the Shiny output
#' @param session the Shiny session
#' @param repo the name of the repo to create the issue in.  e.g. "tychobra/lipids_study"
#' @param gh_pat the GitHub PAT
#'
#' @export
#'
#' @importFrom gh gh
#' @importFrom jsonlite toJSON
#' @importFrom shiny reactiveVal observeEvent showModal modalDialog modalButton actionButton textInput textAreaInput removeModal
#' @importFrom shinyjs onclick
#' @importFrom shinyFeedback showToast loadingButton resetLoadingButton
#'
#'
report_issue_module <- function(
  input,
  output,
  session,
  repo,
  gh_pat
) {
  ns <- session$ns

  open_modal <- shiny::reactiveVal(0)
  shinyjs::onclick(
    "report_bug", {
      open_modal(open_modal() + 1)
    }
  )


  shiny::observeEvent(open_modal(), {

    shiny::showModal(
      shiny::modalDialog(
        title = "Report Issue",
        footer = tagList(
          shiny::modalButton("Cancel"),
          shinyFeedback::loadingButton(
            ns("submit_issue"),
            "Submit",
            class = "btn-primary",
            style = "color: #FFF"
          )
        ),
        shiny::textInput(
          ns("title"),
          "Title",
          width = "100%"
        ),
        shiny::textAreaInput(
          ns("description"),
          "Description",
          width = "100%",
          height = "200px"
        ),
        shiny::fileInput(
          ns("attachments"),
          "Attachments"
        )
      )
    )

  }, ignoreInit = TRUE)


  observeEvent(input$submit_issue, {
    if (is.null(session$userData$user)) {
      hold_email <- "user not signed in"
    } else {
      hold_email <- session$userData$user()$email
    }


    tryCatch({

      body_list <- list(
        title = input$title,
        body = paste0(
          "Created By: ", hold_email,
          "\n\n",
          input$description
        ),
        assignee = "merlinoa"
      )

      body_json <- jsonlite::toJSON(
        body_list,
        auto_unbox = TRUE
      )

      new_issue_res <- gh::gh(
        "POST /repos/{repo}/issues",
        repo = repo,
        .token = gh_pat,
        .send_headers = c(
          Accept = "application/vnd.github.switcheroo-preview+json",
          "Content-Type" = "application/json"
        ),
        charToRaw(body_json)
      )

      shiny::removeModal()

      shinyFeedback::showToast("success", "new issue created")

    }, error = function(err) {

      shinyFeedback::resetLoadingButton("submit_issue")
      msg <- "unable to create issue"
      print(msg)
      print(err)
      shinyFeedback::showToast("error", msg)

      invisible(NULL)
    })



  })

}
