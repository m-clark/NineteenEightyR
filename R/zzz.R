# 80s based ascii art

.onAttach <- function(libname, pkgname) {
  piclist = list(
    "
``````````` ``````````                                                                            ``
````` ````````````````                                                                             `
``````  ``                                                                                        `
``````````````````````````````````````````````````````````````````````````````````  `           `  `
```######+#####++++''+'++++###############++#++++';''';';';''''+++#++++++###########+++++'';;;;;; ``
```+############+#+++++'+++#################+'+++'+',+'';';'''''++###################+++#'';;;;;;  `
```+##############++#++'+#+###################+:'+''';,'''''''++++##################+#++#';;;';;; ``
``#################+++'+#+####@###########++##++''';;';;''''';;+++####################+#''''';'; ``
``########@#######+#++;+#+####@##########++++'+#+'':';;':;''+++;;+###################+#'''''';;;
``#################+#+'+#+#####@#####+##+#+++'+';#''':';'':''++++''####################+''''''';
``#######@#@#######+#+++#+########+###+##+#'+'';'',';+''+''+;++++#++#################+#+''''''';
```#######@#@########+++##+#+#######+##+++#'+;'+'';;':''+;++++#++++####################+'''''''''
```######@#@@########++'##+#+#######++##;+++''''''''''''''+'++++####################@+##'''''''''
```#####@@###@@#####+++'##+#;##++###++###;''#';':'+''''''+'+++##+###################+##+'''''''''
```#####@###@####++++++'#+++:+++++###+'##''''+;;':++''+'''++++++#@#####################+'''''''''
```####@@#@@#####+++'+++++++:'++++#+#++'##:'';';;';'+'''+''+#+#+######################+''''''''''
``###@@@#@######++''+++#++';''++++++++;'++:';'+;'+;;#''''#+++##+###@@#@####@@########+'''''''''' `
``##@@@##@###++#'+''''++++''''++';++++':++';';++;;+';+'''++#++###++#@@##@####@@#@@###'''''''''''
``##@@@#@###+++#'+''''#+'''''''''';''''+'+';;';+'';'+'''''+++#####+#######@###@@##@##'''''''''''
```#@@@@@@##++'++''''''#''';';''''';;+'''+'';;'';''','++'''+++########+#####@@@#@@####''''''''''' `
```#@@@@@@#+++'+'''';;'#'';;':;;;''':'';';';;';+';;'':'+++++++############@##########+''''''''''' `
```#@@@@@++'++'+';';;;;#;';;;:;;;;;;':'';';';;';+':;';:'''++++###+++#######@########@+''''''''''' `
```#@@@@#+++''''';;:;;;+;;;:::;;;:;''':'';;;';;';+';;;;:;''+++++######@@####@######@@@+'''''''''' `
```#@@@#+++'''+;;;;:;;;;:;:::'::;::;;;':';;;;';'''+';;;;;;'''+++++#+####@####@####@@@@@+''''''''' `
```#@@@#+++'';';;;;:::;;';::::::':::;;;':';:;;';''++;:;;;:;';'''++++++####@@##@#@@@@@@@@'''''''''
```#@@#++''';;';;:;:::::#;::::::,::::;;::+;,::;'+'+++;:;:::';;'''''++++####@@##@@@#@@@@@@''''''''
```@@##+''';;;;;::::::::+;::::;:,':::::::;'+::::;+++++;::::;;;';''''++++###@@###@@+@@@@@@@'''''''`
```#@#+''';;;;;;:::::::::::::::,:::::::::::'+;'::;++#+'::::::;';';''''++++#@@@@#@@'#@@@@@@@''''''`
```#@#+''';;;;;':::::::::+::::::::;:::::::::;;++'';''#':::::::;;;;;;;''+++###@@###'+@@@@@@@#'''''``
```#@#+''';;;;;:;:;::::,:';::::;::,:::::::::::::::;';:+::::::::::;;;;;'++#####@@##''@@@@@@@@+''''```
```##++''';;;;;;;:;::::,,:':::,::::::::::::::::::::::::;;,,::::::::;;''+#######@##'''@@@@@@#'+''' `
```##++''';;;;;;;;;;:::::;;:::::;::;:::::::::::::::::,,,:::,::::::;;;########@@#'+''''@@@@@@@+'+'``
```@#+++'''';;;;;;;;;;;+'';;;:;:::;;;;:;::::::::::::::,:::,::::;::+#######@@@##+;+'''''@@@@#@@+'' `
```@#++++'''';;;;;;'+#+++#+##+;::;;:;;;;;;;;:::::::::::::::::;';'+###########++;:+''''''@@@###;+' `
```#@#++++'''';;''+###@@@@@@@@##+'''+'';;;;;;;;;;::::::::::;;++#####@@@@###++';:,:''''''+@@###@#' `
```@@##++++'''''+###@@@@@@@@@@@@@@####+';+;';';;;;;;:;:;;;;;+##@@@@@@@@###++'';:..'''''''+#####:'
```@@@#+++''++++###@@@@@@@@@@@@@@@#####++'++'';;;;;;;;;;;''+###@@@@@@@@@@@##+'::..''''''++#######``
```@@@@'++'''++###@@@@@####@@@@@@@@@@#####+##'';;';;:::;;'+##@@@@@@@###+#@@#++;:.`+'''''+++#####+
```@@@@@'''''+++##############@@@@@@@@@@@####+++';::::::;'##@@@@@@@@#@''+####':,..+''+'''+++@####`
```@@@@@@''''+++####+++#######@@@@@@@@@@#@#####+';::,,,:;'#@@@+####+##:'++#+';:,..+++++++++++####
```#@@@@@@''''++++++++++##@@@@@@'@#@@@@@@@#####+';::,,,,:'#@@@##@@###++'+''';::,..+++++++++++@@##  `
```#@@@@@@+'''''''++++#@@@@@@@@@@''+##@@@@@#####+;::,,.,:;#######+''''++';'';::,,.++++++++++++@#+ ``
```+#@@@@@@''''''''++@@@@##@@@@@@;'###@@######+++;::,..,,;#######+'';;;;''';;::,,.++++++++++##@@@ ``
```+'+@@@@@@''''''++##@@@@##++''++++++########+++;::,...,;++####++'''''';;;;;;:,,.+##@@@@@@@@@@@@
```'''+@@@@@#;;;;;'''++++++++'''++++++#####++++++;::,...,;'++''++''''';;;;;;;:'@@#@@@@@@@@@@@@@@# `
```''''+@@@#@;;;;;;;;''''''';;;;''+++#++++++''''+;::,...,:;'';;;;;;;;;;;;#@@@@@#@@@@@@@@@@@@@@@@#``
```'''';#@@@@@;;;;;;;;;;''''''''++++++''''''''''''::,...,:;;;;;;:;;:;#@@@+:;#@@@@@@@#;;::'#@@@..,``
```'''';;@@@@@+:::;;;;;;;;;;;;;''''''''';;;;'';;;';:,,...,:;;;;:;+@@#;.'@@@@@@#''+'::::;;''#@@@@#`
```'''';;;@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#+';:,,...,:;;;'@@'.:@@@@@+''''++++';;;;''''+@@@@# ``
```''''';;:@#@@@@+;:...,,,:;''++######@@@@@@@#@@@@#@#:....:;#@#.:@@@@#'''''++++++++''''+++++@@@@+``
```''''';;::#@@@@@@@@@@@@@@@@@@@######++##@#@@@@#@@##@@#@#@@'.#@@@#''''+++++++####+++++++++#@@@#+```
```''''';;:::@@@@@@@@@@@@+''''''''''''''''''''+@##@@@@@@###@@@@@@+''+++++++########++#+++###@@@++```
```'+'''';;::#@@``.;@@#''''''+''''++++''''''''''+#@@@@@@@@@@@@@@+++++++++###################@@##+```
```'+'''';;;:#@@@@@@@#+++++++++++++++++++++++++++##@@@@@@@@@@@@#++++++##################+##@@@#++```
```+++''';;;;:'@@@@@#++++++++++++++++++++++++++++###@@@@@@@#@@@############################@@###+``
```+++'''';;;::;@@@@######+#######+++++++++++#+#####@@+;;;;@@@@########################@@@@@@####` `
```++++'''';;;::@@@@#################+#+#+++#+######@@+';;'+@@@#################@@@@@@@@@@@@@####```
```+++++''';;;;;@@@@#############################@##@#;::;;;@@@@#########@@@@@@@@@@@@@@##@@@#####```
```++++++''';;;;+@@@############################@@###;:::::;'@@@@@@@@@@@@@@@@@@@@@@#####@@@######```
```+#+++++''';;;;@@@@######################@@@@@@@##;::::::;:@@@@@@@@@@@@@@@@@@@#######@@@@######` `
```+#++++++'''';;@@@@@@@#######@@@@@@@@@@@@@@@@@@##';::::::::;@@@@@@@@@@@@@@@@@@####+#@@@@#######` `
```+##++++++'''';@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#+;;:::::::;;@@@@@@@@@@@@@@@@@@+++#@@@@#########` `
```+##+++++++''';@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##;;;;:::::;;;'@@@@@@@@@@@@@@@@@#@@@'@@##########  `
```+###+++++++'''+@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#@''';;;;;;;;;;'#@@@@@@@@@@@@@@@@#;+@@############  `
```#####+#+++++'''@@@@@@@@@@@@@@@@@@@#@@@@@@@@@@++'''';;;;;;;''++#@@@@@@@@@@@@@@@@###############```
```########++++++'#@@@@@@@@@####@@#####@@@@@@#@''++''''''''''''+'+++@@@@@@@@@@@################## ``
```##########+++++'@@@@@@@@@@@#####@@@@@@@@@@#''+++++''''''''+++'++#+++++'';;:+##################```
```###########+++++'@@@@@@@@@@@#@#@@@@@@#@#@+++++++++++++++++##++++##++++++';;@################## ``
```###########++++++++#@@@@@@@@@@@#@@@#@#######################++++####++++';'################### ``
```#############+++++++'+#@@@@@@@@@@##++++######################++######+++''+@################## ``
```###############++++++++++++++##++++++++###############################++''#################### ``
```###################+++#+########+++++++###############################++++@################### ``
```##@################################++##################################+++@################### ``
```##@####################################################################++@####################```
```###@@@##@@################@#############################################+#####################```
```@@@@@@@@@############################################################################@##@@####``
```#@@@@@@@@@@@#############@@@@@@@@@#####################################################@@#####``
```#@@@@@@@@@@@@@##@#@@#####@@@@@@@@@###########################@##################@@@@@##@@#####``
```#@@@@@@@@@@@@@@@@@@@@####@@@@@@@@@@#######################@#@@@#########@@#####@@@@#@@@@@@@### ``
```#@@@@@@@@@@@@@@@###@@@###@#@##@@@@@#######################@@@@@@#######@@@@##@@@@@@@@@@@@@@###```
```#@@@@@@@@@@@@@@@@@@@@###########@@#@##########################@@########@@@@@@@@@@@@@@@@@@@###```
```##@@@@@@@@@@@@@@@@@@@############@#@@#####################@##@########@@@@@@@@@@@@@@@@@@@@@###```
```##@@@@@@@@@@@@@@@@@@@#######################################@##########@@@@@@@@@@@@@@@@@@@@@##```
```##@@@@@@@@@@@@@@@@@@@@##################################################@@@##@@@@@@@@@@@@@@###```
```##@@@@@@@@@@@@@@@@@@@@#################################################@@@@@#@@@@@@@@@@@@@@@##` `
```##@@@@@@@@@@@@@@@@@@@@##################################################@@@##@@@@@@@@@@@@@@@##` `",
    "
..,,,,,..,,:;''+''''''''''''''''''''''''''''+''''+''+'''''''''''''''''''''''''''''''''''''''''''''''
..,,,,...,,::'++++''''''''''''''''''''''+''''''''''++''''''''''''''''''''''''''';'''''''''''''''''''
..,,,.....,:;'+++''''''''''''''''''''''''+'''''''''''''''''''''''''''''';;'''''';''''''''';'''''''''
..,,,,...,,:;'''+''''''''''''''''''''''''''''''''''';;;+''''''''''''''';;;''''';;;''';''''''';;;;;;'
...,,.....,,;'''''''''''''''''''''''''''''''''''+####'';;'''''''''';;;;;;;;;;';;;;;';'''';;;;;;;;;;;
...,,,...,,,;'''''''''''''''''''''''''''''''''+++##@###'+:'''''''';''';;;;;;';;;;;;'';;;;;';;;;;;;;;
..,,,,,....,;'''''''''''''''''''''''''''''';'++#'+###@+###';'''''''''';';;;;';;;;'';;;;'''';;;;;;;;;
,,,,,,,....,;''''''''''''''''''''''''''''';'+++'+#'##@#@#@+''''''';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
,,::,,,,,.,,;'''''''''''''''''''''''''''''+##'++:+#@+##@@@##+'''';;;;';;;;;;;;;;;;;;;;;;;';;;;;;;;;;
,,:::,,,,,,:;'''''''';;;'''''''''''''''''++++#''@++#'#+@@@#++'''';;'';;;;;;;;;;;';'''';;'''''';;''';
.,,,,:,,,,,:;''''''''';;'''''''''''''';'''+';;'++++######@@#+++'';;''';;;;;;;;;;;;'''';;;''''';;''''
..,,,,,,,,,:;''''''''';;''''''''''''''';''+'::;''';'++++###@+'''';'''';';;;;;;;;;;'''';;'''''';;''''
.,,,,,,,,,,:;''''''''';'''''''''''''''''++';:,.,::,,::;;'++'+;'''''''''';;;;;;;;;;''''';'''''';'''''
..,,,,,,,,,:;'''''''''';;'''''';'''''''''+':,,,....,,:::;;:';;''''';''';;;;;;;;;;;;'''';'''''';;''''
..,,,,,,,,,:;''''''''''''''''''''''''''''#;:,,,.`.,,:::;;:.:';'''''';'';;;;;;;;''';'''';'''''';;''''
...,,,,,,,,:;''''''''''''''''''''''''''''#':,,..`.,:,::;';.:;''''''';'''';;;;;'''';';''''''''';'''';
..,,,,,,,,,:;'''''''''''''''''''''''''''+#':,,...,.:,::;'':,;;;'''''';;''';;';'';'''''''';';';';;';;
,,,,,,,,,,,,;'''''''''''''''''''''''''''++':,,..`.,,,::;''::;:''''';;';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
:;;;;::,,,,,;''''''''''''''''''''''''''''+',......,:,,:;;',;;:''''';;'';;;;;;;;';;;;;;;;;;;;;;;;;;;;
;;;;;;:::,,::'''+''''''''''''''''''''''',+;,......,:,,:;';:;';'''''''''';;;;'''';;;;;;;;;;;;;;;;;;;;
:;;;;;::::::;'''++'';'''''''''''''''''':.',,,.,,,,:;:,:;''::';''''''''''';;'''';;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;::;''''+'''''''''''''''''''';':,'.,,:''+;:;::;+#+:,;:+''''''''';;;''''';;;;;;;;;;;;;;;;;;;;
:::;:;;;;;;;''''++''''''''''''''''''';':.;,.,::,'',.;#####'.:''+''''''''';;'''''';;;;;;;;;;;;;;;;;;;
,,:::::;::;'''''++''''''''''''''''''''':,.,.....::,.'#+''+'.::''''''''''';;'''''';;;;;;;;;;;;;;;;;;;
.,,:,::::::;'''''''''''''''''''''''''''',.,....,::..'#+;'';.;;'''''''''''';'''''';;;;;;;;;';;;;;';;;
..,,,,,,,,:;;'''''''''''''''''''''''''''..,,...,.`,.;+#';';.';''''''';'';';'''';;';;';;'';;;;;;''';'
.,,,:,,:,,,:;''''''''''''''''''''''''''',.,,,.....,,:'++;;:::;'''''';;''';''''''';;;;;';;;;;;;;;;;;'
,,:::::::,::;'''''''''''''''''''''''''''':,,,...,.,.:'+++';;;''''';;;;;;;';';';;';;;;;;;;;;;';;;;;;;
::;;:::::::;;''''''''''''''''''''''''''''#,,,...:.,,',+++';:;';'';;;;;;;;;;;;';;;;;;;;;;;;;''';;'';;
::;::::;;:;;''''+'''''';;''''+'''''''''''#:,,......:'#+++';:;'''';;;;;;'';;''';;;;;;;;;;'''';'';''';
,::::::::::;;'''+++'''''''+++++''''''''''';,....,.,;'+++';;:;;''';;;;;''''''';;;;;;;;;;;''''''''''';
,::::,:::::;;'''+++'''';''+++++''''''''''';,...,,.,,:+''';;;;;'''';;;;'''''';';;;;;;;;;'''''''';'';;
,,:::,,:,,::;'''+++''''''''++++'''''''''''',,.,.,,;;'+#+;;;;;;;'';;;;;''';';;;;;;;;;;;;;'';'';'';;;;
.,,,,,,,,,,:;''''''''''''''''+''''''''''''',,,,,,:;''+++;';;'''''';;;;'''';;;;;;;;;;;;;;;;;;;';';;;;
.,,,,,,,,,,:;''''''''''''''''''''''''''''';,:,,,,,:;;+++''`'''''''''';'''''''';';;;;;;;;;;';;;;;;;;;
..,,,,,,,,,:;'''''''''''''''''''''''''''''`,,::,,,::''''';`,'''''''''''''''''';;;;;'''';;;;;;';';;;;
..,,,,,,,,,::'''''''''''''''''''''''''''''`.,,::::':'+''+:``''''''''''''';;';';;;;;;;;;';;;;;;;;;;;;
..,,,,,,:,,::;'''''''''''''''''''''''''';:``,,,::;';'''#+.``;'''''''';''''';';'';;;;';;;';;''';;;';;
..,,,,::,,,::''++''''''''''''+'''''''''';```.,,,::;+++##:...`;''''''''''''';'';;;;';;;;;;'''';';;;;'
.,,,,,,,,,,::'++''''''''''''''''''''''';`````:,,::'+'++;....`::''''';;'''';;;';;;;';,,:;;''''';;;;;;
.,,,,,,,,,,::'+''''''''''''''+'''''''''```````;,,:;;;:``.....;;''''';;''''';;'';;'':...;;;'''';;;';'
..,,,,,,,,,::''+'''''''''''''''''''';+`````````::::;;``````..,,:@+'';;'''''';;;;;;;:...;;;'''';;;'''
..,,,,,,,,,,:'''+'''''''''''''''''''#+`.`````.'```;`,+#'.`````;:#@'@#.;''''';;;;;;;:..,;;;;''';;;;;;
..,,,,,,,,,,:''''''''''''''''''',`#:#...```.+'+.,``;+####+;.``;;:@@+@'.`;;;;:;;;;;;:...;;;;;';;';;;;
..,,,,,,,,,,:''''''''''''''''';`.#'+#`.```+'':;':;##'@##:,,,:`';:#@:##....:;:::::;;:...;;;;;';;;';;;
..,,,,,,,,,,:''''''''''''''''``.:#,#'`.`,,,;`..;'+#@@#:,,,,:,,,;'#@:@#,,,,..::::::::...:;;;;;;;';;;;
.,,,,,,,,,,::'''''''''''''':``..+#,+.........,,.''###;,,,,,,,,,,:+@;##;.,,,,.:::::::...:;;;;;;;;;;;;
.,,,,,:,,,,::'''''''''''''```...#+'+``.........,+#@+:.,::,,,,,,:,:#+'#+.,,,,,..........:';;;;;;'';;;
..,,,,:,:,,::''''''''''''``.....+;#+`.........,.++#+.;..,,,,::;;::##:##.,,,,:,`.``````.:';;;;;;';;;;
..,,,,,,,,,::'''''''''''.`.`...:+,#+`.........,,++#+;...,,,,,,,,,:@#,##,,,,,:,,```````.:';;;;;;;';;;
.,,,,,,,,,,::'''''''''''`..`...'+.++`..........'#;#+:.,,.,,,.,,::.##,@#.,,,,;:,.``````.:';;;;;;;;;;;
.,,,,,,,,,,,:''+'''''''.``.....+#,+:..........:'#'##:......,,,:,,.##:##:,,,,:',:``````.:';;;;;;'';;;
.,,,,,.,,,,,:''''''''''`.``.`..++:+...........:'#@@#,,.....,,,,,,,'#'+#;:,:,;',.``````.:';;;;;;'';;;
..,,....,,,,:'''''''''.`.``.`..++'+`..........:'####.....,,,,,,,.,;@#;##,:::;':,``````.:;;;;;';;'';;
.,......,,,,:;''';;;;:`.``.....+;++`.........:+#@###,,...,,,,,,,,,;##:##,:,;:',,.`````.,;;;;;;;;;'';
,,,......,,,,,,,,:,,,,`,.`....,+,++........,::+#++##',...,,,,,,,,,:#@,##,:,':',:.`````.,;;;;;;;;;';;
,,,,...,,.,,,,,,,,,,.``,.`,..`:+.+;.......,,.;##++##+.....,,,,.,,,:#@:@#,::;:',:,......,;;;';;;;'';'
,,,,.,.,,,,,,,,,,,,,,``...:,..;+.+,....:,.,..'+#####+,...,,,,,..,,:'#;@@,:;;:',;,......,;;';;'''''''
,,,,...,,,,,,,,,,,,,,``...:,``'+,+..`:,......'+#+#+'#'.....,,,,.,,:;@'##,+'::':;:..,,,,:;;';''''''''
,,,,...,,,,,,,,,,,,,,`.,..:..`'+:+..,.......,######+#+.,.....,,,,::;@#+@,++::':;:.,,,,,:''';;'''''''
,,,,,,,,,,,,,,,,,,,,.``:.....`'+'+,........,,+#+++#;#+:.......,,,::;@@+@;'+::;:;:.,,,,:;;'';;;;'''''
,,,,,,,,,,,,,,,,,,,,```;.....`''';`.......,,,++#######+........,,:;;#@+@'+',:;:;;,,,,,:;''''''''''''
,,,,,,,,,,,,,,,,,,,,```'...;.`+++``......,,.:;'#:#++#++;.......'+';;'#+@+#+.,;:;;,,,,,:;;''';'''''''
,,,,,,,,,,,,,,,,,,,,```;...,..+;+;`.........''####+'##++......,;:,;'`+':+++.,:::;:.,,,:;;;''';'';';;
,,,,,,,,,,,,,,,,,,,:...:....:`',++.`........'+#''#####++,,....,::::;:##'++#.,:::;:.,,::;;;;;;;;;;;;;
,,,,,,,,,,,,,,,,,,,,..`.:...:,+,+'..........+'';'+'##+++'.,..,,::::;#@'@++#.,:::;:..````````````````
,,,,,,,,,,,,,,,,,,,,....'...,;;:+:........,.++';'+##+++++.,,.,,::::;#@+@###,,:;:;;,:::::::::::::::;;
''''''''''''''''''',....+....;:;+:.......,,.''++++;+##+++..,,,,,::;:#@#@##@:,::;';:;;';;';;''';';;;;
;;;:::;::::::::::::`....;...,;`'';.......:,.+++;+',:#++++,,,,,,,::;;#@#@@@@;,:;;;;:.,,,:''''''''+'''
,,,,,,,,,,,,,,,,,,,`.....:..,:.+';......,,..#+##+';+##++',,,,,,:::''#@@@@@@',:;:;;:.,,,:''''''''''''
,,,,,,,,,,,,,,,,,,,`.....+.,::`+;+......:,..###++++###++',,,,,,,;;;+@@@@@@@+,,;:;;:,,,,;''''''''''''
,,,,,,,,,,,,,,,,,,,`.....'.,:;`+,+.....,:,.,+@#;+#:###+++;,,,,,:;':#@@@@#@@+,:;:;;::,::;;;;;;;;;;;;;
,,,,,,,,,,,,,,,,,,:......::,:'.+.+`,``,,,,.,###@@@####+++:,,,,,:;':@@@@@#@##:,;:';:::::,,:;;:::;::::
,,,,,,,,,,,,,,,,,,:......,',:;.',+:,`.,,,,,.''#@@@####++:.,,,,,:;+;'@@@@#@##;,;;;;::'''''';+'''';'''
,,,,,,,,,,,,,,,,,,,.......',:..;,++.`.,,....;'###@###++'.,,,,,,;:'';@@@#+@##;,';';:;;'''''''''+''+''
,,,,,,,,,,,,,,,,,,........;':``:;+#``.:....,;;+###@@#++'..,,,,:;:'+;@@@#+@##;,;;';:;:'';;';''''';'':
,,::::::::::::::::........:''``:'+#``,,....;+'';;+;##++,..,:;;;:;'+'@#@#+@@#;:;;';;:,;;';;;;;;+:;';:
::::::::::::::::,:`......,,''`.,+'+`.,.....'#++';'+'+'....,::::,:;++@#@@+@##::;'';;:,'';'''':;+;;+;'
:::;;;;;;:;:::::,:`......,:'+`..+:+`.......';'+##';'#:,...,:;:::;:+#@#@#####;:;;':;::;+';';+':+;:'+'
;;;;;;;;;;;::::,::`......,:;+..`+,+.`......'::+;#':;#:,,..,:;:::;;+@@#@#+###;:;;;:;;:;;++'''+'++'+#'
:::::::;;;;:::,,,:`......,::#..`+.+,.......':;+######'....,:::;;;;+#@+@##@@#;:;':;;;:+;;'++++#++###+
,::,,,::;;::::,,,:`......,::+..``.``.......'''';+####+....,:';;;;;'':::+#:@@::;;:;;;:;+':;+#++####+#
,,,,,,,:;;::::,,,:.......,::'.``````.....`.';';::+;##'.....:;''';;':,,,#@:##::;:::';;,;++';;+#######
,,,,,,,:;;::,,,,,,.......,;;;+``````,....`.+''+;'+###',....:;;;';'';,,,##:@#::;;;'''',::'++';'+#++++
,,::::,:;;::,:,,,....,,,..;''+:..```.``..`,;;'++++''#',....;;;;';'++,,'##:#@::'';;';;,'::;'++++++'+'
:,:'':::;;:,,,,,,`.......,;''+:'.``.`...`.:',:+;+'::@'....,;'''''++#:,#'+,@@,;;;::::::++';;'##++++''
:,:'':::;';,,,,,,`......,,,';;''``.`.`....:'::++'+;'#'...,;;'''+#@#+;:'':,@@,:::,:;'';;'+++''+++++++
:::'';::;';,,,,,,`........,;';:.`,...,....;''+'''+++#+,..;'''''+;,;;'';::,#@:;;;;;:::;:;:'+#++++++++
:::;;;::;';:,,,,,.........,;;,...:,.`:,..,;+''',,+:+#+,..;;;;:,.:;;''@++:,;@,;;;'''':;:::;;''++#+++#
:::::::::::,,,,,,...`...,,,;+`..`.........;+''':;+''#+;,,:,,,,,::;;#@##@#:,@,;;:,::,:;:::::;';+++###
,,,,,,:,,,,,,,,,,.`..,.....;+.,.``......,.;++''+++;'#+';''';;,;;::'''''#@@+@,;;::,,;';:';;;'''+++++#
,,,,,,,,,,,,,,,,,`..,,.,,,.:;...`....`....:#';+'+',:##++;:,,.;'::+'''##+++@@::,,,,:;';,+#+';''++++##
:::::::::::::,,,:.........,,;.`.`....`....,#';++++;;##++'..,:'::'+'#+''++'+#;:,:::::;',';;'+;;+++###
::::::::::::,,,,:.......,,,::``.`.........,+''+#+++++#++;,,,;:;;;''+##++++'#+'::,,,:::::;;'';'++++##
,,,,,,,,,,,,,,,,,...........:...`..;:.....,'++#;'##+###+,,,;;:;';++++@+'#+:#++,:;:,;;+.::;''''+++###
'''''''''''''''''`.,..,,:;:,,'.,`..:,.....:'+##'+@#####+,::,:'++'+++@+''##'###'.,,:+:.:::;;;''+++##@
''';;'''''';:::::`....,,::,,;'',.`..;,`...;+@+#+++''+#+':,:'++''+;+#+''++;'##+;,,::.:;;:;;:'''++###@
'';::;'''';:,,,,,`....,.,,::'+;;+':,.:`...''::+++',,+#++++++''''+;#+#@@@+;+@@@#:,.,:;';;;''';'#+###@
'';;:;'''';:,,,,,.`...,,:,,,,,;;++++++;:;;'';:++#+;:+@+++'''++#@#@#@@@@@+;+##+:,,,,:';:++'''';#+##@@
;;;;:;;;;;;:,,,,,.`,,..,::,:+#;'+++++++++''++##''@@@#@@@@@@@@@@@@@@@@@@@;@@##+:,:,,::'+:+++++++###@@
;;;::;;;;;::,,,,,,..`.,,,:::;#';++';''+''''+;#+'+#'@@@@@@@@@@@@@@@@@@@@@'@@@#',:.:,,,::,:;''''+##@@@
'':,,::;;;;:,,,,,,``.....;+;++'+++'++''';'''+##'#@@@@@@@@@@@@@@@@@@@@@@@#@@#+,.,..,;';::':;'+;;'+#@@
'';::;'''';,,,,,,,.,....:;:'##'+''++''''';;';'++#@@@@@@@@@@@@@@@@@@@@@@@@@@@;.,.,,;:''':;+'';'+++###
+';:::';;::,,:,,::.`....,::+#+#+;+;;''''';'+'''+#@@@@@@@@@@@@@@+#@####@@@@@@,.:::::'++';+;;;'+';###@
';:,,,,,::;;;:::::.'..,,::;++#+;#;';;';;'@+''''+##@@@@@@@@@@@#@##@@@@@@#@@@#.,.,,,,+'++';;''':;+###@
,,:::::;:,,,::,,::.+:.,::,:++#;;#'';''''#+''''''+'+@@@@@@@##@@#'#@@@#@@@'#@+,.,,.::'''+;':;;;''##@@@
;;;;;'';::::::;;;;,,,,,,,,:+#';#;;;;'';;@'+'+####'++@@@@@++@@+++@@@@#@@@##@@....,::'';;;::'';;#+#@@@
;;'''+;';:,..,,,::;;,,,,;::++';@;;;;;;;'@#''+''+#+#+#@@@++###++#@@@@@##@@+@@#..,,:';:;';''''####@@@@
;;;;;;;;;;;::;'+#@@##:,,,,:#'+;#;';;;;;#@+++''+'+##+@###'+#++'+#@+#@####@#@@@::'#++;;''+#+';'##+##@@
;;;;;;;;;;;:#@##+#@@@:,,,,:'+'+;;;';';;@@'++'';++'''#';';+#+'+@'@'#@@###@@+@##:;'+'::;'';:;++++#####
;;;;;;;;;;;:+###++@@@;:,,,''+'@;;;;'';;@@;''+++''+''+''+'#+'+@@'#'@#@@###@+@@#+;'';''';;;''++'#####@
;;;;;;;;;;;;'##+++#@@#::,,;++;@;';;';;'@@'''''+#''''+''+'+'+@@'''+@##@@##@@###''';;'''';;'#+++####@#
;;;;;;;;;;;;;##+++#@@@::,:;++;;;;'';:;#@+;''';''++;'+''++++@@#'##+@###@@##@'@#':;:::::;'##'+++'###@@
;;;;;;;;;;;;:######@@@;:,;'''#'';;';:'@@:;'''''''+''+'+#++@@@''@#+++###@########':;::;'+''+++'###@@@
;;;;;;;;;;;;:#####+@@@':,;'+;#;;;;';;;@@:''''''''+''#'+##@@@+'+@#''+###@@##@+@#+;:;''++';'+++###@@@@
;;;;;;;;;;;;:#####+@@@+::;'+;;;;;;;;';@#;''''''''+;'#++#@@@#''#@'';+###+@###+####;;;;'';'++++###@@@#
;;;;;;;;;;;;:+####+#@@@:;;''+;;;;;;;'##+;;;''''''';+#'#@@@@+;'#@;''##@#@#@######+':::::'+++++###@@@@
;;;;;;;;;;;;;'@###+#@@@:;;'++;;;;;';;@++;'''';''';;++'+@@@#''+@@;'+@###@@@@######;,,::'+'''++##@@@##
;;;;;;;;;;;;;;@###+#@@@:;;'';';;;'';;'+''+''';'''''##+#@@@#''+@@'++##@@#@+#######::::;+;'++++#####@#
;;;;;;;;;;;;;:@@##++@@@'';'''';;;'';''+'''''';'''+++#+#@@@''+#@@'+##@@@@@#+##+#@#;:::';;'++####@@@##
;;;;;;;;;;;;;:@###+#@@@#';;';';;;'';+#++'+''';'''++###@@@#;'+#@@'+##@@###@@+@#@##;::;;;#'+####@##@@@
;;;;;;;;;;;;;:#@###+@@@@'';';;;;'';'#+++++''';'''+####@@@+;+####'++##@@@#@##+####;:::++''++####@@#@@
;;;;;;;;;;;;;:+@####@@@@';;;;:;;';;'#++++++++;++######@@+''##@##++###@###@#@#+###':'++;''+######@@##
;;;;;;;;;;;;;:'@@@##@@@@';;;;;'''';'+++'++##++#@@@@#@#@#+##@@@@##@@@#@@@@@#@@####''#;+';++####@##@@@
;;;;;;;;;;;;;;;@@##@@@##';;;;''++#+####+######@@@@@@@#@@@@@@@@@@@@@@@@@@@@#@@@###'+'':;'++####@#@#@@
;;;;;;;;;;;;;;:#@@#+';;;';;;'+###@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##@@#@@##+''++:;++####@@@@@#
;;;;;';;;;;;;;:+';;;;''''';';+####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#########@@@@@#;#+';;++###@@#@@@@
;;;;;;;;;;;;;;:;;''++#+;''++''++++####@@@@@@@@@@@@@@@@@@@@@@@@@#+##+++#+#+++@@@@@#++''++####@##@@@@@
';';;;;;;;;;;;'++##@@@#''++#''''''++++###@@@@@@@@@@@@@@@@@@@####+#####@##++#@@@@@####+';#######@#@@@
+++++'''++'+++###@#+'++'++''''''''++++++###@#@@@@@@@@@@@@#@#@@@@@@######+#@@@@@@@##@++;++###@##@@@@@
@@@@@@@#####@@@@@@;;;;+++#++'+''';'''+++++++'##@@@@@@@@@@@@@@@@#######+###@@@@@@@##@++;'####@##@@@@@
#@+++++#@@@@@@##+#;;;;+'++++'+++''+''''''''''''+'+@@#@@@#@#'++++#####+++##@@@@@@@@@@#+;'####@@@@@@@@
'#'''++@@@@+''''''''''+'++++'++#+'+++++++++++###@@@@++''''';'+++++++++++##@@@@@@@@@@#+''##@@@#@@@@@@
+#'++@@@#'''''''++''''+'+++++++##'#++++#@@@@@@@@@@@@##+++'+'+'''++'++++##@@@@@@@@@@@@+;'###@@#@@@@#@",

    "
           ^^                   @@@@@@@@@
      ^^       ^^            @@@@@@@@@@@@@@@
                           @@@@@@@@@@@@@@@@@@              ^^
                          @@@@@@@@@@@@@@@@@@@@
~~~~ ~~ ~~~~~ ~~~~~~~~ ~~ &&&&&&&&&&&&&&&&&&&& ~~~~~~~ ~~~~~~~~~~~ ~~~
~         ~~   ~  ~       ~~~~~~~~~~~~~~~~~~~~ ~       ~~     ~~ ~
  ~      ~~      ~~ ~~ ~~  ~~~~~~~~~~~~~ ~~~~  ~     ~~~    ~ ~~~  ~ ~~
  ~  ~~     ~         ~      ~~~~~~  ~~ ~~~       ~~ ~ ~~  ~~ ~
~  ~       ~ ~      ~           ~~ ~~~~~~  ~      ~~  ~             ~~
      ~             ~        ~      ~      ~~   ~             ~
    ",
    "

                                                           `..
                                                         .....,.,`
                                                          `.,,,,,.....`
                                               ```         ,::,,...,.........`
                                    ``.........,,...,..`. .,::     ,::,......
                                  .+:..........,..........:,:;     .,::  .,.
                              `.';;:,,'+:......```,;.;...:,......`,,:;.
                        #   `'::,,,;;::::;#..@''+'''#`+#@#,,.......,##,
                        # ,';';:::::::::;.`;@+#+++;,+.,'@@@,.........``,':
                    ````:;'+::::::::::,#`.@';;#@+'''':`@;@##..`.............
                 ``````.+;;'#;::;::::;,`:;;;+++''';:;',.``...........,,,,,,.
               `...``...`.'##;;;'';;#`.@+@:';;;'+:```.`................,,,,,
             `...`..........@';';;:+.+@@@#+:.`````.....#@#........``....,,,,
           ``................,...``````,............+@####,.....``..::'.,,,
          .,;:`..............`````````..............``..@@.....```####@#,:
         ..;................` ````````........................``.@#.,,##,
        ..`...``.........`` ``` ``````.,.....................``.,#:;:,##,
        ,..,..`.......:.` ```` `,@##@+`....................,;;;;@;,,,,,.
         ,;.,....`..,,`````````@#:.`##@...........,;:,,....,....#.,#,,;`
        `..:'.:.....`````.;`.`.#`.:..@@`.,;:,...............,..@#.,,..@
         ..,,.';`...``````..``@#'..@.`@@...................,;:@@@@....;
         .,.,,..`..``......``.#:...`. #@,........``.;+@@@######@@#@'@#.
         .:;',..,..```.``````@#;..```,+@,..:'@@@@@@@@######+++''+@#++:.
          `,;'++`..,   `````.@#@:`.``@@@@@@#####+++';;:,.`
             .:'+#;  ```````@@@#+`  @+###++';:.
               `,;'####@###@@@@#####+'';.
                   .;'++++++''';'#'.



    "

  )

  chance = stats::runif(1)
  if(chance > .5 && length(base::find.package('png', quiet=T) > 0)) {
    # tempDir <- tempfile()
    # dir.create(tempDir)
    # htmlFile <- file.path(tempDir, "vignettes/onload.html")
    # viewer <- getOption("viewer")
    # viewer(htmlFile)
    # rstudioapi::viewer(htmlFile)
    display_198R()
  }
  else if(chance <= .01){
    packageStartupMessage(sample(piclist[1:2], 1)[[1]])
  }
  else{
    packageStartupMessage(sample(piclist[-(1:2)], 1)[[1]])
  }
}


.onLoad <- function(libname, pkgname) {
  op <- options()
  op.198R <- list(

  )
  toset <- !(names(op.198R) %in% names(op))
  if(any(toset)) options(op.198R[toset])

  # invisible()
}
