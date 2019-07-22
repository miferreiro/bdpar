context("pipeline_execute")

test_that("pipeline_execute configurationFilePath type error",{

  #Path where the configuration file are located
  configurationFilePath <-  1

  editConfigurationFile <- FALSE

  #Folder with the files to preprocess
  filesPath <- system.file("testFiles_pipeline_execute_tsms",
                           package = "bdpar")

  #Object which indicates the pipes' flow
  pipe <- SerialPipe$new()

  #Object which decides how creates the instances
  instanceFactory <- InstanceFactory$new()


  expect_error(pipeline_execute(configurationFilePath = configurationFilePath,
                                editConfigurationFile = editConfigurationFile,
                                filesPath = filesPath,
                                pipe = pipe,
                                instanceFactory = instanceFactory),"\\[pipeline_execute\\]\\[Error\\]
              Checking the type of the variable: configurationFilePath numeric")
})


test_that("pipeline_execute configurationFilePath extension error",{

  #Path where the configuration file are located
  configurationFilePath <-  "example.ext"

  editConfigurationFile <- FALSE

  #Folder with the files to preprocess
  filesPath <- system.file("testFiles_pipeline_execute_tsms",
                           package = "bdpar")

  #Object which indicates the pipes' flow
  pipe <- SerialPipe$new()

  #Object which decides how creates the instances
  instanceFactory <- InstanceFactory$new()


  expect_error(pipeline_execute(configurationFilePath = configurationFilePath,
                                editConfigurationFile = editConfigurationFile,
                                filesPath = filesPath,
                                pipe = pipe,
                                instanceFactory = instanceFactory),"\\[pipeline_execute\\]\\[Error\\]
              Checking the extension of the file: configurationFilePath ext")
})

test_that("pipeline_execute editConfigurationFile type error",{

  #Path where the configuration file are located
  configurationFilePath <-  system.file("configurations",
                                        "test_pipeline_execute_tsms_configurations.ini",
                                        package = "bdpar")

  editConfigurationFile <- NULL

  #Folder with the files to preprocess
  filesPath <- system.file("testFiles_pipeline_execute_tsms",
                           package = "bdpar")

  #Object which indicates the pipes' flow
  pipe <- SerialPipe$new()

  #Object which decides how creates the instances
  instanceFactory <- InstanceFactory$new()


  expect_error(pipeline_execute(configurationFilePath = configurationFilePath,
                                editConfigurationFile = editConfigurationFile,
                                filesPath = filesPath,
                                pipe = pipe,
                                instanceFactory = instanceFactory),"\\[pipeline_execute\\]\\[Error\\]
            Checking the type of the variable: editConfigurationFile NULL")
})

# test_that("pipeline_execute file_edit !is.null(configurationFilePath) ",{
#
#   #Path where the configuration file are located
#   configurationFilePath <-  system.file("configurations",
#                                         "test_pipeline_execute_tsms_configurations.ini",
#                                         package = "bdpar")
#
#   editConfigurationFile <- TRUE
#
#   #Folder with the files to preprocess
#   filesPath <- system.file("testFiles_pipeline_execute_tsms",
#                            package = "bdpar")
#
#   #Object which indicates the pipes' flow
#   pipe <- SerialPipe$new()
#
#   #Object which decides how creates the instances
#   instanceFactory <- InstanceFactory$new()
#
#   mock = mockery::mock(TRUE)
#   mockery::stub(svMisc::file_edit, 'file_edit', mock)
#
#   expect_length(pipeline_execute(configurationFilePath = configurationFilePath,
#                                  editConfigurationFile = editConfigurationFile,
#                                  filesPath = filesPath,
#                                  pipe = pipe,
#                                  instanceFactory = instanceFactory),2)
#
#   file.remove("output_tsms.csv")
# })


test_that("pipeline_execute filesPath type error",{

  #Path where the configuration file are located
  configurationFilePath <-  system.file("configurations",
                                        "test_pipeline_execute_tsms_configurations.ini",
                                        package = "bdpar")

  editConfigurationFile <- FALSE

  #Folder with the files to preprocess
  filesPath <- NULL

  #Object which indicates the pipes' flow
  pipe <- SerialPipe$new()

  #Object which decides how creates the instances
  instanceFactory <- InstanceFactory$new()


  expect_error(pipeline_execute(configurationFilePath = configurationFilePath,
                                editConfigurationFile = editConfigurationFile,
                                filesPath = filesPath,
                                pipe = pipe,
                                instanceFactory = instanceFactory),"\\[pipeline_execute\\]\\[Error\\]
            Checking the type of the variable: filesPath NULL")
})


test_that("pipeline_execute pipe type error",{

  #Path where the configuration file are located
  configurationFilePath <-  system.file("configurations",
                                        "test_pipeline_execute_tsms_configurations.ini",
                                        package = "bdpar")

  editConfigurationFile <- FALSE

  #Folder with the files to preprocess
  filesPath <- system.file("testFiles_pipeline_execute_tsms",
                           package = "bdpar")

  #Object which indicates the pipes' flow
  pipe <- NULL

  #Object which decides how creates the instances
  instanceFactory <- InstanceFactory$new()


  expect_error(pipeline_execute(configurationFilePath = configurationFilePath,
                                editConfigurationFile = editConfigurationFile,
                                filesPath = filesPath,
                                pipe = pipe,
                                instanceFactory = instanceFactory),"\\[pipeline_execute\\]\\[Error\\]
            Checking the type of the variable: pipe NULL")
})

test_that("pipeline_execute instanceFactory type error",{

  #Path where the configuration file are located
  configurationFilePath <-  system.file("configurations",
                                        "test_pipeline_execute_tsms_configurations.ini",
                                        package = "bdpar")

  editConfigurationFile <- FALSE

  #Folder with the files to preprocess
  filesPath <- system.file("testFiles_pipeline_execute_tsms",
                           package = "bdpar")

  #Object which indicates the pipes' flow
  pipe <- SerialPipe$new()

  #Object which decides how creates the instances
  instanceFactory <- NULL


  expect_error(pipeline_execute(configurationFilePath = configurationFilePath,
                                editConfigurationFile = editConfigurationFile,
                                filesPath = filesPath,
                                pipe = pipe,
                                instanceFactory = instanceFactory),"\\[pipeline_execute\\]\\[Error\\]
            Checking the type of the variable: instanceFactory NULL")
})

if (Sys.info()[['sysname']] %in% "Windows") {
test_that("pipeline_execute default flow of pipes with the examples files tsms",{


  #Path where the configuration file are located
  configurationFilePath <-  system.file("configurations",
                                        "test_pipeline_execute_tsms_configurations.ini",
                                        package = "bdpar")

  #Folder with the files to preprocess
  filesPath <- system.file("testFiles_pipeline_execute_tsms",
                          package = "bdpar")

  #Object which indicates the pipes' flow
  pipe <- SerialPipe$new()

  #Object which decides how creates the instances
  instanceFactory <- InstanceFactory$new()

  #Starting file preprocessing...
  output <- suppressWarnings(pipeline_execute(configurationFilePath = configurationFilePath,
                             filesPath = filesPath,
                             pipe = pipe,
                             instanceFactory = instanceFactory))
  file1 <- output[[1]]

  expect_equal(file1$getDate(),"")

  expect_equal(file1$getSource(),"Wait that's still not all that clear, were you not sure about me being sarcastic or that that's why x doesn't want to live with us\r\n")

  expect_equal(file1$getData(),"wait that's still not all that clear, were you not sure about me being sarcastic or that that's why x doesn't want to live with us")
  expect_equal(file1$getSpecificProperty("target"),"ham")
  expect_equal(file1$getSpecificProperty("extension"),"tsms")

  expect_equal(file1$getSpecificProperty("length_before_cleaning_text"),132)

  expect_equal(file1$getSpecificProperty("userName"),as.character(c()))
  expect_equal(file1$getSpecificProperty("hashtag"),as.character(c()))
  expect_equal(file1$getSpecificProperty("URLs"),as.character(c()))
  expect_equal(file1$getSpecificProperty("emoticon"),as.character(c()))
  expect_equal(file1$getSpecificProperty("Emojis"),as.character(c()))
  expect_equal(file1$getSpecificProperty("language"),"en")
  expect_equal(file1$getSpecificProperty("contractions"),list())
  expect_equal(file1$getSpecificProperty("abbreviation"),list())
  expect_equal(file1$getSpecificProperty("langpropname"),list())
  expect_equal(file1$getSpecificProperty("interjection"),list())
  expect_equal(file1$getSpecificProperty("stopWord"),list())
  expect_equal(file1$getSpecificProperty("length_after_cleaning_text"),130)
  expect_equal(file1$isInstanceValid(),TRUE)
  expect_equal(file1$getFlowPipes(),list("TargetAssigningPipe",
                                         "StoreFileExtPipe",
                                         "GuessDatePipe",
                                         "File2Pipe",
                                         "MeasureLengthPipe",
                                         "FindUserNamePipe",
                                         "FindHashtagPipe",
                                         "FindUrlPipe",
                                         "FindEmoticonPipe",
                                         "FindEmojiPipe",
                                         "GuessLanguagePipe",
                                         "ContractionPipe",
                                         "AbbreviationPipe",
                                         "SlangPipe",
                                         "ToLowerCasePipe",
                                         "InterjectionPipe",
                                         "StopWordPipe",
                                         "MeasureLengthPipe",
                                         "TeeCSVPipe"))

  expect_equal(file1$getBanPipes(),c("FindUrlPipe","FindHashtagPipe","AbbreviationPipe"))

  file2 <- output[[2]]

  expect_equal(file2$getDate(),"")

  expect_equal(file2$getSource(),"Guess what! Somebody you know secretly fancies you! Wanna find out who it is? Give us a call on 09065394514 From Landline DATEBox1282EssexCM61XN 150p/min 18\r\n")

  expect_equal(file2$getData(),"guess what! somebody you know secretly fancies you! wanna find out who it is? give us a call on 09065394514 from landline datebox1282essexcm61xn 150p/min 18")
  expect_equal(file2$getSpecificProperty("target"),"spam")
  expect_equal(file2$getSpecificProperty("extension"),"tsms")

  expect_equal(file2$getSpecificProperty("length_before_cleaning_text"),158)

  expect_equal(file2$getSpecificProperty("userName"),as.character(c()))
  expect_equal(file2$getSpecificProperty("hashtag"),as.character(c()))
  expect_equal(file2$getSpecificProperty("URLs"),as.character(c()))
  expect_equal(file2$getSpecificProperty("emoticon"),as.character(c()))
  expect_equal(file2$getSpecificProperty("Emojis"),as.character(c()))
  expect_equal(file2$getSpecificProperty("language"),"en")
  expect_equal(file2$getSpecificProperty("contractions"),list())
  expect_equal(file2$getSpecificProperty("abbreviation"),list())
  expect_equal(file2$getSpecificProperty("langpropname"),list())
  expect_equal(file2$getSpecificProperty("interjection"),list())
  expect_equal(file2$getSpecificProperty("stopWord"),list())
  expect_equal(file2$getSpecificProperty("length_after_cleaning_text"),156)
  expect_equal(file2$isInstanceValid(),TRUE)
  expect_equal(file2$getFlowPipes(),list("TargetAssigningPipe",
                                         "StoreFileExtPipe",
                                         "GuessDatePipe",
                                         "File2Pipe",
                                         "MeasureLengthPipe",
                                         "FindUserNamePipe",
                                         "FindHashtagPipe",
                                         "FindUrlPipe",
                                         "FindEmoticonPipe",
                                         "FindEmojiPipe",
                                         "GuessLanguagePipe",
                                         "ContractionPipe",
                                         "AbbreviationPipe",
                                         "SlangPipe",
                                         "ToLowerCasePipe",
                                         "InterjectionPipe",
                                         "StopWordPipe",
                                         "MeasureLengthPipe",
                                         "TeeCSVPipe"))
  expect_equal(file2$getBanPipes(),c("FindUrlPipe","FindHashtagPipe","AbbreviationPipe"))

  file.remove("output_tsms.csv")

})
}
#
#
# # test_that("default flow of pipes with the examples files eml",{
# #
# #   #Path where the configuration file are located
# #   configurationFilePath <-  system.file("configurations",
# #                                         "test_pipeline_execute_eml_configurations.ini",
# #                                         package = "bdpar")
# #
# #   #Folder with the files to preprocess
# #   filesPath <- system.file("testFiles_pipeline_execute_eml",
# #                            package = "bdpar")
# #
# #   #Object which indicates the pipes' flow
# #   pipe <- SerialPipe$new()
# #
# #   #Object which decides how creates the instances
# #   instanceFactory <- InstanceFactory$new()
# #
# #   #Starting file preprocessing...
# #   output <- pipeline_execute(configurationFilePath = configurationFilePath,
# #                              filesPath = filesPath,
# #                              pipe = pipe,
# #                              instanceFactory = instanceFactory)
# #
# #   file3 <- output[[1]]
# #
# #   expect_equal(is.na(file3$getDate()),TRUE)
#   # expect_equal(file3$getSource(),"b'<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">\\n<HTML><HEAD>\\n<META http-equiv=Content-Type content=\"text/html; charset=windows-1252\">\\n<META content=\"MSHTML 6.00.2716.2200\" name=GENERATOR>\\n<STYLE></STYLE>\\n</HEAD>\\n<BODY bgColor=#ffffff>\\n<DIV><FONT size=2>I\\'m using Simple DNS from JHSoft.&nbsp; We support only a few \\nweb sites and I\\'d like to swap secondary services with someone in a similar \\nposition.</FONT></DIV>\\n<DIV><FONT size=2></FONT>&nbsp;</DIV>\\n<DIV><FONT size=2>We have a static IP, DSL line and a 24/7 set of web, SQL, mail \\nand now a DNS server.&nbsp; As I said, we are hosting about 10 web sites, web \\nand DNS traffic is almost nothing.&nbsp; Everything is on lightly loaded APC \\nbattery backups so we are very seldom down.</FONT></DIV>\\n<DIV><FONT size=2></FONT>&nbsp;</DIV>\\n<DIV><FONT size=2>I\\'d like to swap with someone also using Simple DNS to take \\nadvantage of the trusted zone file transfer option.</FONT></DIV>\\n<DIV><FONT size=2></FONT>&nbsp;</DIV>\\n<DIV><FONT size=2></FONT>&nbsp;</DIV>\\n<DIV><FONT size=2></FONT>&nbsp;</DIV>\\n<DIV><FONT size=2>Bob Musser<BR>Database Services, Inc.<BR>Makers \\nof:<BR>&nbsp;&nbsp; Process Server\\'s Toolbox<BR>&nbsp;&nbsp; Courier Service \\nToolbox<BR><A href=\"mailto:BobM@dbsinfo.com\">BobM@dbsinfo.com</A><BR><A \\nhref=\"http://www.dbsinfo.com\">www.dbsinfo.com</A><BR>106 Longhorn Road<BR>Winter \\nPark FL 32792<BR>(407) 679-1539</FONT></DIV>\\n<DIV>&nbsp;</DIV>\\n<DIV><FONT size=2></FONT>&nbsp;</DIV></BODY></HTML>\\n'")
#   # expect_equal(file3$getData(),"b'<!doctype html public \"-//w3c//dtd html 4.0 transitional//en\">\\n<html><head>\\n<meta http-equi content-type content=\"text/htm  charse windows-1252\">\\n<meta content=\"mshtml 6.00.2716.2200\" nam generator>\\n<style></style>\\n</head>\\n< dy bgcolor=#ffffff>\\n<div><font siz 2>i\\'m using simple dns from jhsoft.&nbs  we support only a few \\nweb sites and i\\'d like to swap secondary services with someone in a similar \\nposition.</font></div>\\n<div><font siz 2></font>&nbs </div>\\n<div><font siz 2>we have a static ip, dsl line and a 24/7 set of web, sql, mail \\nand now a dns server.&nbs  as i said, we are hosting about 10 web sites, web \\nand dns traffic is almost nothing.&nbs  everything is on lightly loaded apc \\nbattery backups so we are very seldom down.</font></div>\\n<div><font siz 2></font>&nbs </div>\\n<div><font siz 2>i\\'d like to swap with someone also using simple dns to take \\nadvantage of the trusted zone file transfer option.</font></div>\\n<div><font siz 2></font>&nbs </div>\\n<div><font siz 2></font>&nbs </div>\\n<div><font siz 2></font>&nbs </div>\\n<div><font siz 2>bob musser<br>database services, inc.<br>makers \\no <br>&nbsp;&nbs  process server\\'s toolbox<br>&nbsp;&nbs  courier service \\ntoolbox<br><a href=\"mailto >bobm@dbsinfo.com</a><br><a \\nhre  \" </a><br>106 longhorn road<br>winter \\npark fl 32792<br>(407) 679-1539</font></div>\\n<div>&nbs </div>\\n<div><font siz 2></font>&nbs </div></ dy></html>\\n'")
#   # expect_equal(file3$getSpecificProperty("target"),"ham")
#   # expect_equal(file3$getSpecificProperty("extension"),"eml")
#   # expect_equal(file3$getSpecificProperty("length_before_cleaning_text"),1511)
#   # expect_equal(file3$getSpecificProperty("userName"),as.character(c()))
#   # expect_equal(file3$getSpecificProperty("hashtag"),as.character(c()))
#   # expect_equal(file3$getSpecificProperty("URLs"),c(UrlPattern1 = "http://www.dbsinfo.com",
#   #                                                  UrlPattern2 = "www.dbsinfo.com",
#   #                                                  EmailPattern = "BobM@dbsinfo.com"))
#   # expect_equal(file3$getSpecificProperty("emoticon"),c("v=",
#   #                                                      "l;",
#   #                                                      "t=",
#   #                                                      "e=",
#   #                                                      "BO",
#   #                                                      "p;",
#   #                                                      "f:",
#   #                                                      "f="))
#   # expect_equal(file3$getSpecificProperty("Emojis"),as.character(c()))
#   # expect_equal(file3$getSpecificProperty("language"),"en")
#   # expect_equal(file3$getSpecificProperty("contractions"),list())
#   # expect_equal(file3$getSpecificProperty("abbreviation"),list())
#   # expect_equal(file3$getSpecificProperty("langpropname"),list())
#   # expect_equal(file3$getSpecificProperty("interjection"),list())
#   # expect_equal(file3$getSpecificProperty("stopWord"),list())
#   # expect_equal(file3$getSpecificProperty("length_after_cleaning_text"),1427)
#   # expect_equal(file3$isInstanceValid(),TRUE)
#   # expect_equal(file3$getFlowPipes(),list("TargetAssigningPipe",
#   #                                        "StoreFileExtPipe",
#   #                                        "GuessDatePipe",
#   #                                        "File2Pipe",
#   #                                        "MeasureLengthPipe",
#   #                                        "FindUserNamePipe",
#   #                                        "FindHashtagPipe",
#   #                                        "FindUrlPipe",
#   #                                        "FindEmoticonPipe",
#   #                                        "FindEmojiPipe",
#   #                                        "GuessLanguagePipe",
#   #                                        "ContractionPipe",
#   #                                        "AbbreviationPipe",
#   #                                        "SlangPipe",
#   #                                        "ToLowerCasePipe",
#   #                                        "InterjectionPipe",
#   #                                        "StopWordPipe",
#   #                                        "MeasureLengthPipe",
#   #                                        "TeeCSVPipe"))
#   #
#   # expect_equal(file3$getBanPipes(),c("FindUrlPipe","FindHashtagPipe","AbbreviationPipe"))
#   #
#   #
#   #
#   #
#   # file4 <- output[[2]]
#   #
#   # expect_equal(is.na(file4$getDate()),TRUE)
#   # expect_equal(file4$getSource(),"b'<html>\\n<head>\\n<title>Save 84% on CE Credits</title>\\n<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">\\n</head>\\n\\n<body bgcolor=\"#FFFFFF\" text=\"#000000\">\\n<p align=\"center\">\\n  <table width=\"557\" border=\"3\" cellspacing=\"0\" cellpadding=\"0\" bordercolor=\"003399\" height=\"709\">\\n    <tr>\\n      <td height=\"703\" align=\"center\"> \\n          <table width=\"550\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\">\\n            <tr> \\n              <td align=\\'center\\'>\\n                <img src=\"http://iiq.us/images/iqcamp/header2.gif\" width=\"550\" height=\"128\" alt=\\'Save 84% on C.E. Credits & Pre-Licensing Courses\\'><br>\\n                <img src=\"http://iiq.us/images/iqcamp/images.jpg\" width=\"550\" height=\"121\"><br>\\n                <img src=\"http://iiq.us/images/iqcamp/tag2.gif\" width=\"550\" height=\"130\" alt=\\'State CE, CLU, CFP, CPA and Securities\\'><br>\\n                <img src=\"http://iiq.us/images/iqcamp/24_months.gif\" width=\"235\" height=\"86\" alt=\\'Unlimited courses & credits for 24 months\\'><br>\\n                <img src=\"http://iiq.us/images/iqcamp/price2.gif\" width=\"550\" height=\"42\" alt=\\'Regular Price:$1298, IQ Price:$199\\'>\\n              </td>\\n            </tr>\\n          </table>\\n          <table width=\"550\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\">\\n            <tr> \\n              <td align=\"center\"> \\n                <table width=\"480\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\">\\n                  <tr> \\n                    <td width=\"242\"> \\n                      <b><font size=\"2\" face=\"Verdana, Arial, Helvetica, sans-serif\">\\n                      <ul>\\n                          <li>No Hidden Fees</li>\\n                          <li>Over 16,000 Credits</li>\\n                          <li>Over 300 Courses</li>\\n                      </ul>\\n                      </font></b>\\n                    </td>\\n                    <td width=\"269\"> \\n                      <b><font size=\"2\" face=\"Verdana, Arial, Helvetica, sans-serif\">\\n                      <ul>\\n                          <li>90% Internet Based</li>\\n                          <li>High Quality Courses</li>\\n                          <li>Unlimited Access</li>\\n                      </ul>\\n                      </font></b>\\n                    </td>\\n                  </tr>\\n                </table>\\n              </td>\\n            </tr>\\n          </table>\\n          <table width=\"550\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\">\\n            <tr> \\n              <td height=\"136\" align=\"center\">\\n                  <table width=\"550\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\">\\n                    <tr> \\n                      <td> \\n                        <table width=\"100%\" bgcolor=\"#FFFFFF\">\\n                          <tr> \\n                            <form method=\"post\" action=\"http://65.217.159.103/response/response.asp\">\\n                              <td> \\n                                <table width=\"100%\" border=\"0\" cellspacing=\"0\" cellpadding=\"2\" align=\"center\" bgcolor=\"#ffffff\">\\n                                  <tr bgcolor=\"003399\"> \\n                                    <td colspan=\"5\" align=\"center\"><font size=\"2\" face=\"Verdana, Arial, Helvetica, sans-serif\" color=\"#FFFFFF\"><b>Complete this form for a FREE Guest Demo!</b></font></td>\\n                                  </tr>\\n                                  <tr> \\n                                    <td width=\"20%\" align=\"right\"><font face=\"Verdana, Arial, Helvetica, sans-serif\" size=\"2\"><b>Name:</b></font></td>\\n                                    <td colspan=\"4\"><input type=\"text\" name=\"contactname\" size=\"50\"></td>\\n                                  </tr>\\n                                  <tr> \\n                                    <td width=\"20%\" align=\"right\"><font face=\"Verdana, Arial, Helvetica, sans-serif\" size=\"2\"><b>E-mail:</b></font></td>\\n                                    <td colspan=\"4\"><input type=\"text\" name=\"email\" size=\"50\"></td>\\n                                  </tr>\\n                                  <tr> \\n                                    <td width=\"20%\" align=\"right\"><font face=\"Verdana, Arial, Helvetica, sans-serif\" size=\"2\"><b>Phone:</b></font></td>\\n                                    <td colspan=\"4\"><input type=\"text\" name=\"phone\" size=\"50\"></td>\\n                                  </tr>\\n                                  <tr> \\n                                    <td width=\"20%\" align=\"right\"><font face=\"Verdana, Arial, Helvetica, sans-serif\"><b>City:</b></font></td>\\n                                    <td width=\"25%\"><input type=\"text\" name=\"city\" size=\"20\"></td>\\n                                    <td width=\"15%\" align=\"right\"><font face=\"Verdana, Arial, Helvetica, sans-serif\" size=\"2\"><b>State:</b></font></td>\\n                                    <td width=\"20%\"><input type=\"text\" name=\"state\" size=\"2\"></td>\\n                                    <td rowspan=\"2\" align=\"center\" valign=\"middle\" width=\"20%\"><img src=\"http://iiq.us/images/q2.gif\" width=\"43\" height=\"50\"></td>\\n                                  </tr>\\n                                  <tr> \\n                                    <td width=\"20%\" align=\"right\">&nbsp;</td>\\n                                    <td width=\"25%\" colspan=\\'3\\'> \\n                                      <input type=\"submit\" name=\"btnsubmit\" value=\"Submit\">\\n                                      <input type=\"hidden\" name=\"hdnRecipientTxt\" value=\"iqcampus@insiq.com\">\\n                                      <input type=\"hidden\" name=\"hdnSuccessPageTxt\" value=\"http://insiq.financialcampus.com\">\\n                                      <input type=\"hidden\" name=\"hdnSubjectTxt\" value=\"IQ Campus Inquiry\">\\n                                    </td>\\n                                  </tr>\\n                                </table>\\n                              </td>\\n                            <input type=\\'hidden\\' name=\\'SentTo\\' value=\\'304704\\'>\\n</form>\\n                          </tr>\\n                        </table>\\n                      </td>\\n                    </tr>\\n                  </table>\\n                  <img src=\"http://iiq.us/images/iqcamp/logo.gif\" width=\"286\" height=\"65\">\\n              </td>\\n            </tr>\\n          </table>\\n          <font face=\"Verdana, Arial, Helvetica, sans-serif\" size=\"1\" color=\"#000000\">We \\n          don\\'t want anybody to receive our mailing who does not wish to receive \\n          them. To be removed from this mailing list, DO NOT REPLY to this message. \\n          Instead, go here: <a href=\"http://www.InsuranceIQ.com/optout\">http://www.InsuranceIQ.com/optout</a><br>\\n          <a href=\"http://www.insuranceiq.com/legal.htm\">Legal Notice</a></font>\\n      </td>\\n    </tr>\\n  </table>\\n</p>\\n</body>\\n</html>\\n'")
#   # expect_equal(file4$getData(),"b'<html>\\n<head>\\n<title>save 84% on ce credits</title>\\n<meta http-equiv=\"content-type\" content=\"text/htm  charse iso-8859-1\">\\n</head>\\n\\n<body bgcolo   tex  >\\n<p align=\"center\">\\n  <table width=\"557\" border=\"3\" cellspacing=\"0\" cellpadding=\"0\" bordercolor=\"003399\" height=\"709\">\\n    <tr>\\n      <td height=\"703\" align=\"center\"> \\n          <table width=\"550\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\">\\n            <tr> \\n              <td align=\\'center\\'>\\n                <img sr   width=\"550\" height=\"128\" alt=\\'save 84% on c.e. credits & pre-licensing courses\\'><br>\\n                <img sr   width=\"550\" height=\"121\"><br>\\n                <img sr   width=\"550\" height=\"130\" alt=\\'state ce, clu, cfp, cpa and securities\\'><br>\\n                <img sr   width=\"235\" height=\"86\" alt=\\'unlimited courses & credits for 24 months\\'><br>\\n                <img sr   width=\"550\" height=\"42\" alt=\\'regular price 1298, iq price 199\\'>\\n              </td>\\n            </tr>\\n          </table>\\n          <table width=\"550\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\">\\n            <tr> \\n              <td align=\"center\"> \\n                <table width=\"480\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\">\\n                  <tr> \\n                    <td width=\"242\"> \\n                      <b><font size=\"2\" face=\"verdana, arial, helvetica, sans-serif\">\\n                      <ul>\\n                          <li>no hidden fees</li>\\n                          <li>over 16,000 credits</li>\\n                          <li>over 300 courses</li>\\n                      </ul>\\n                      </font></b>\\n                    </td>\\n                    <td width=\"269\"> \\n                      <b><font size=\"2\" face=\"verdana, arial, helvetica, sans-serif\">\\n                      <ul>\\n                          <li>90% internet based</li>\\n                          <li>high quality courses</li>\\n                          <li>unlimited access</li>\\n                      </ul>\\n                      </font></b>\\n                    </td>\\n                  </tr>\\n                </table>\\n              </td>\\n            </tr>\\n          </table>\\n          <table width=\"550\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\">\\n            <tr> \\n              <td height=\"136\" align=\"center\">\\n                  <table width=\"550\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\">\\n                    <tr> \\n                      <td> \\n                        <table width=\"100%\" bgcolo  >\\n                          <tr> \\n                            <form method=\"post\" actio  \">\\n                              <td> \\n                                <table width=\"100%\" border=\"0\" cellspacing=\"0\" cellpadding=\"2\" align=\"center\" bgcolo  >\\n                                  <tr bgcolor=\"003399\"> \\n                                    <td colspan=\"5\" align=\"center\"><font size=\"2\" face=\"verdana, arial, helvetica, sans-serif\" colo  ><b>complete this form for a free guest demo!</b></font></td>\\n                                  </tr>\\n                                  <tr> \\n                                    <td width=\"20%\" align=\"right\"><font face=\"verdana, arial, helvetica, sans-serif\" size=\"2\"><b>nam </b></font></td>\\n                                    <td colspan=\"4\"><input type=\"text\" name=\"contactname\" size=\"50\"></td>\\n                                  </tr>\\n                                  <tr> \\n                                    <td width=\"20%\" align=\"right\"><font face=\"verdana, arial, helvetica, sans-serif\" size=\"2\"><b>e-mai </b></font></td>\\n                                    <td colspan=\"4\"><input type=\"text\" name=\"email\" size=\"50\"></td>\\n                                  </tr>\\n                                  <tr> \\n                                    <td width=\"20%\" align=\"right\"><font face=\"verdana, arial, helvetica, sans-serif\" size=\"2\"><b>phon </b></font></td>\\n                                    <td colspan=\"4\"><input type=\"text\" name=\"phone\" size=\"50\"></td>\\n                                  </tr>\\n                                  <tr> \\n                                    <td width=\"20%\" align=\"right\"><font face=\"verdana, arial, helvetica, sans-serif\"><b>cit </b></font></td>\\n                                    <td width=\"25%\"><input type=\"text\" name=\"city\" size=\"20\"></td>\\n                                    <td width=\"15%\" align=\"right\"><font face=\"verdana, arial, helvetica, sans-serif\" size=\"2\"><b>stat </b></font></td>\\n                                    <td width=\"20%\"><input type=\"text\" name=\"state\" size=\"2\"></td>\\n                                    <td rowspan=\"2\" align=\"center\" valign=\"middle\" width=\"20%\"><img sr   width=\"43\" height=\"50\"></td>\\n                                  </tr>\\n                                  <tr> \\n                                    <td width=\"20%\" align=\"right\">&nbs </td>\\n                                    <td width=\"25%\" colspan=\\'3\\'> \\n                                      <input type=\"submit\" name=\"btnsubmit\" value=\"submit\">\\n                                      <input type=\"hidden\" name=\"hdnrecipienttxt\" valu  >\\n                                      <input type=\"hidden\" name=\"hdnsuccesspagetxt\" valu  \">\\n                                      <input type=\"hidden\" name=\"hdnsubjecttxt\" value=\"iq campus inquiry\">\\n                                    </td>\\n                                  </tr>\\n                                </table>\\n                              </td>\\n                            <input type=\\'hidden\\' name=\\'sentto\\' value=\\'304704\\'>\\n</form>\\n                          </tr>\\n                        </table>\\n                      </td>\\n                    </tr>\\n                  </table>\\n                  <img sr   width=\"286\" height=\"65\">\\n              </td>\\n            </tr>\\n          </table>\\n          <font face=\"verdana, arial, helvetica, sans-serif\" size=\"1\" colo  >we \\n          don\\'t want anybody to receive our mailing who does not wish to receive \\n          them. to be removed from this mailing list, do not reply to this message. \\n          instead, go her  <a hre  \" </a><br>\\n          <a hre  \">legal notice</a></font>\\n      </td>\\n    </tr>\\n  </table>\\n</p>\\n</body>\\n</html>\\n'")
#   # expect_equal(file4$getSpecificProperty("target"),"spam")
#   # expect_equal(file4$getSpecificProperty("extension"),"eml")
#   # expect_equal(file4$getSpecificProperty("length_before_cleaning_text"),6796)
#   # expect_equal(file4$getSpecificProperty("userName"),as.character(c()))
#   # expect_equal(file4$getSpecificProperty("hashtag"),c("#FFFFFF","#000000","#ffffff"))
#   # expect_equal(file4$getSpecificProperty("URLs"),c(UrlPattern1 = "http://iiq.us/images/iqcamp/header2.gif\"",
#   #                                                  UrlPattern2 = "http://iiq.us/images/iqcamp/images.jpg\"",
#   #                                                  UrlPattern3 = "http://iiq.us/images/iqcamp/tag2.gif\"",
#   #                                                  UrlPattern4 = "http://iiq.us/images/iqcamp/24_months.gif\"",
#   #                                                  UrlPattern5 = "http://iiq.us/images/iqcamp/price2.gif\"",
#   #                                                  UrlPattern6 = "http://65.217.159.103/response/response.asp",
#   #                                                  UrlPattern7 = "http://iiq.us/images/q2.gif\"",
#   #                                                  UrlPattern8 = "http://insiq.financialcampus.com",
#   #                                                  UrlPattern9 = "http://iiq.us/images/iqcamp/logo.gif\"" ,
#   #                                                  UrlPattern10 = "http://www.InsuranceIQ.com/optout",
#   #                                                  UrlPattern11 = "http://www.insuranceiq.com/legal.htm",
#   #                                                  EmailPattern = "iqcampus@insiq.com"))
#   # expect_equal(file4$getSpecificProperty("emoticon"),c())
#   # expect_equal(file4$getSpecificProperty("Emojis"),as.character(c("l;",
#   #                                                                 "t=",
#   #                                                                 "r=",
#   #                                                                 "c=",
#   #                                                                 ":$",
#   #                                                                 "n=",
#   #                                                                 "e:",
#   #                                                                 "l:",
#   #                                                                 "y:",
#   #                                                                 "p;",
#   #                                                                 "e=",
#   #                                                                 "f=")))
#   # expect_equal(file4$getSpecificProperty("language"),"en")
#   # expect_equal(file4$getSpecificProperty("contractions"),list())
#   # expect_equal(file4$getSpecificProperty("abbreviation"),list())
#   # expect_equal(file4$getSpecificProperty("langpropname"),list())
#   # expect_equal(file4$getSpecificProperty("interjection"),list())
#   # expect_equal(file4$getSpecificProperty("stopWord"),list())
#   # expect_equal(file4$getSpecificProperty("length_after_cleaning_text"),6261)
#   # expect_equal(file4$isInstanceValid(),TRUE)
#   # expect_equal(file4$getFlowPipes(),list("TargetAssigningPipe",
#   #                                        "StoreFileExtPipe",
#   #                                        "GuessDatePipe",
#   #                                        "File2Pipe",
#   #                                        "MeasureLengthPipe",
#   #                                        "FindUserNamePipe",
#   #                                        "FindHashtagPipe",
#   #                                        "FindUrlPipe",
#   #                                        "FindEmoticonPipe",
#   #                                        "FindEmojiPipe",
#   #                                        "GuessLanguagePipe",
#   #                                        "ContractionPipe",
#   #                                        "AbbreviationPipe",
#   #                                        "SlangPipe",
#   #                                        "ToLowerCasePipe",
#   #                                        "InterjectionPipe",
#   #                                        "StopWordPipe",
#   #                                        "MeasureLengthPipe",
#   #                                        "TeeCSVPipe"))
#   # expect_equal(file4$getBanPipes(),c("FindUrlPipe","FindHashtagPipe","AbbreviationPipe"))
#
#   # file.remove("output_eml.csv")
#
# # })
