

#' Report Issue Module UI
#'
#' @param id the module ID
#'
#' @export
#'
#' @importFrom shiny NS tags
#' @importFrom shinyFeedback useShinyFeedback
#'
#' @return "Report Issue" button to go in the top bar of a shinydashboard Shiny app.
#'
report_issue_module_dash_ui <- function(id) {
  ns <- shiny::NS(id)

  tags$li(
    class = "dropdown",
    tags$head(shinyFeedback::useShinyFeedback()),
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
#' @param gcs_bucket_name the Google Cloud Storage bucket name
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
  gh_pat,
  gcs_bucket_name
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
            class = "btn btn-primary",
            style = "color: #FFF; width: 150px;"
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
          "Attachments",
          multiple = TRUE
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

    hold_attachments <- input$attachments
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

      if (!is.null(hold_attachments)) {
        # upload attachments to Google Cloud Storage and include links to the
        # attachments in the markdown sent with the GitHub Issue
        md_links <- c()
        file_url <- paste0("gh_attachment/", uuid::UUIDgenerate())
        for (i in seq_len(nrow(hold_attachments))) {

          the_row <- hold_attachments[i, ]
          gcs_upload_res <- googleCloudStorageR::gcs_upload(
            file = the_row$datapath,
            bucket = gcs_bucket_name,
            name = file_url,
            predefinedAcl = "publicRead"
          )

          md_links[i] <- paste0("[", the_row$name, "](https://storage.googleapis.com/", gcs_bucket_name, "/", file_url, ")")
        }

        md_links_string <- paste(md_links, collapse = "\n")

        body_list$body <- paste0(body_list$body, "\n\n Attachments: \n", md_links_string)
      }


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
