protectwords <- function(txt){
  txt <- unprotectwords(txt)
  txt <- gsub(txt,pattern="(natalipr|nobelpr)(ize)",replacement="\\1xXx\\2")
  txt <- gsub(txt,pattern="(col)(ors[ ]*=)",replacement="\\1xXx\\2")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(al)(uminum)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(ar)(tifact)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(ana|cata|hydro|para)(lyz)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(ly)(zing)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(di)(arrhea)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(de|of)(fense)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(fu)(ror)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(gr)(ay)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(je)(welry)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(ma)(neuver)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(pe)(diatric)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(pe)(dophil)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(sk)(eptic)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(in)(quiry)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(va)(por)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(on)(stage)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(break|clean|lock|pick)(up)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(he)(mo)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(or)(thopedic)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(ar)(cheo)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(pa)(leonto)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(en)(ology)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(es)(ophag)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(es)(trogen)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(es)(thetic)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(ho)(meopath)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(me)(dieval)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(ri)(gor)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(an|leuk|septic|tox)(emi)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(su)(lfur)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(lik|liv|rat|sal|siz|shak)(able)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(clam|glam|harb|neighb|rum|savi?)(or)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(behavi|col|fl?av|hon|hum|lab)(or)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(mo)(ld)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(calib|fib|goit|lust|mit|nit|reconnoit|sab|saltpet|spect|theat|tit)(er)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(cent|epicent|recent)(er)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(me)(ter)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(centi|milli|deci|pico|hecto)(liter)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(dema|peda|mono|syna)(gog)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(ana|cata|dia|epi|homo|mono|pro)(log)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(iz)(e[drs]?|ations?|ing)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(el)(e[dr]|est|ing)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(counci|dia)(le[dr]|ling|lor)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(app|enthr|riv|sign|tot)(ale[dr]|aling)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(<.+?>[^<]+?)(iz)(e[drs]?|ations?|ing)([^<]+?<\\/.+?>)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(al)(uminum)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(ar)(tifact)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(ana|cata|hydro|para)(lyz)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(ly)(zing)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(di)(arrhea)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(de|of)(fense)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(fu)(ror)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(gr)(ay)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(je)(welry)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(pe)(diatric)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(pe)(dophil)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(sk)(eptic)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(in)(quiry)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(va)(por)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(on)(stage)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(break|clean|lock|pick)(up)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(he)(mo)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(or)(thopedic)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(ar)(cheo)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(pa)(leonto)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(en)(ology)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(es)(ophag)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(es)(trogen)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(es)(thetic)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(ho)(meopath)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(me)(dieval)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(ri)(gor)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(an|leuk|septic|tox)(emi)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(su)(lfur)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(lik|liv|rat|sal|siz|shak)(able)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(clam|glam|harb|neighb|rum|savi?)(or)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(behavi|col|fl?av|hon|hum|lab)(or)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(mo)(ld)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(calib|fib|goit|lust|mit|nit|reconnoit|sab|saltpet|spect|theat|tit)(er)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(cent|epicent|recent)(er)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(centi|kilo|milli|\\d\\s)(meter)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(centi|milli|deci|pico|hecto)(liter)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(dema|peda|mono|syna)(gog)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(ana|cata|dia|epi|homo|mono|pro)(log)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(iz)(e[drs]?|ations?|ing)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(el)(e[dr]|est|ing)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(counci|dia)(le[dr]|ling|lor)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[Category:[^\\]]*?)(app|enthr|riv|sign|tot)(ale[dr]|aling)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[de:[^\\]]*?)(spiel)(er)([^\\]]*?\\])",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(al)(uminum)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(ar)(tifact)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(ana|cata|hydro|para)(lyz)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(ly)(zing)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(di)(arrhea)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(de|of)(fense)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(fu)(ror)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(gr)(ay)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(je)(welry)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(ma)(neuver)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(pe)(diatric)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(pe)(dophil)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(sk)(eptic)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(in)(quiry)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(va)(por)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(on)(stage)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(break|clean|lock|pick)(up)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(he)(mo)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(or)(thopedic)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(ar)(cheo)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(pa)(leonto)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(en)(ology)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(es)(ophag)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(es)(trogen)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(es)(thetic)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(ho)(meopath)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(me)(dieval)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(ri)(gor)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(an|leuk|septic|tox)(emi)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(su)(lfur)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(lik|liv|rat|sal|siz|shak)(able)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(clam|glam|harb|neighb|rum|savi?)(or)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(behavi|col|fl?av|hon|hum|lab)(or)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(mo)(ld)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(calib|fib|goit|lust|mit|nit|reconnoit|sab|saltpet|spect|theat|tit)(er)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(cent|epicent|recent)(er)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(centi|kilo|milli|\\d\\s)(meter)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(centi|milli|deci|pico|hecto)(liter)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(dema|peda|mono|syna)(gog)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(ana|cata|dia|epi|homo|mono|pro)(log)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(iz)(e[drs]?|ations?|ing)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(bev|jew|lev|mod|rev|trav)(el)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})([a-z]{2,8}[^egl\b])(el)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(counci|dia)(le[dr]|ling|lor)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\[(?:file|image):[^\\.]{0,20})(app|enthr|riv|sign|tot)(ale[dr]|aling)([^\\.]{0,20}\\.)",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(al)(uminum)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(ar)(tifact)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(ana|cata|hydro|para)(lyz)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(ly)(zing)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(di)(arrhea)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(de|of)(fense)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(fu)(ror)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(gr)(ay)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(je)(welry)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(ma)(neuver)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(pe)(diatric)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(pe)(dophil)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(sk)(eptic)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(in)(quiry)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(va)(por)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(on)(stage)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(break|clean|lock|pick)(up)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(he)(mo)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(or)(thopedic)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(ar)(cheo)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(pa)(leonto)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(en)(ology)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(es)(ophag)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(es)(trogen)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(es)(thetic)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(ho)(meopath)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(me)(dieval)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(ri)(gor)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(an|leuk|septic|tox)(emi)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(su)(lfur)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(lik|liv|rat|sal|siz|shak)(able)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(clam|glam|harb|neighb|rum|savi?)(or)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(behavi|col|fl?av|hon|hum|lab)(or)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(mo)(ld)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(calib|fib|goit|lust|mit|nit|reconnoit|sab|saltpet|spect|theat|tit)(er)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(cent|epicent|recent)(er)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(centi|kilo|milli|\\d\\s)(meter)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(centi|milli|deci|pico|hecto)(liter)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(dema|peda|mono|syna)(gog)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(ana|cata|dia|epi|homo|mono|pro)(log)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(iz)(e[drs]?|ations?|ing)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(el)(e[dr]|est|ing)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(counci|dia)(le[dr]|ling|lor)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt <- gsub(txt,pattern="(\\{[^{]{0,6}quot[^\\|]{1,7}\\s?\\|[^}]*?)(app|enthr|riv|sign|tot)(ale[dr]|aling)([^}]*?})",replacement="\\1\\2xXx\\3\\4")
  txt
}

Simple <- function(txt){
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/ ])aluminum",replacement="\\1aluminium")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])artifact",replacement="\\1artefact")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])(ana|cata|hydro|para)lyz(e|ing)",replacement="\\1\\2lys\\3")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])diarrhea",replacement="\\1diarrhoea")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])(de|of)fense",replacement="\\1\\2fence")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])furor([^\\w\\d\\-\\/])",replacement="\\1furore\\2")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])(light-|dark-|\\s)gray",replacement="\\1\\2grey")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])judgment",replacement="\\1judgement")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])jewelry",replacement="\\1jewellery")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])maneuver(ab|ed|ing)",replacement="\\1manoeuvr\\2")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])maneuver",replacement="\\1manoeuvre")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])ped(iatric|ophil)",replacement="\\1paed\\2")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])skeptic",replacement="\\1sceptic")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])in(quir(?:e[sd]|ing|y))",replacement="\\1en\\2")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])vapor(s?[^\\w\\d\\-\\/])",replacement="\\1vapour\\2")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])(break|lock|pick)up",replacement="\\1\\2-up")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])gyneco",replacement="\\1gynaeco")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])hemo(globin|ly|phil)",replacement="\\1haemo\\2")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])orthopedic",replacement="\\1orthopaedic\\2")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])archeo",replacement="\\1archaeo")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])paleonto",replacement="\\1palaeonto")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])enology",replacement="\\1oenology")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])esophag",replacement="\\1oesophag")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])estrogen",replacement="\\1oestrogen")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])esthetic",replacement="\\1aesthetic")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])homeopath",replacement="\\1homoeopath")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])rigor([^\\w\\d\\-\\/])",replacement="\\1rigour\\2")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])(an|leuk|septic|tox)emi(a|c)",replacement="\\1\\2aemi\\3")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])sulfur",replacement="\\1sulphur")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])(lik|liv|rat|sal|siz|shak)(able)",replacement="\\1\\2e\\3")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])(clam|glam|harb|neighb|rum|savi?)or(ed|ful|ing|less|ly|s|)([^\\w\\d\\-\\/])",replacement="\\1\\2our\\3\\4")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])neighborhood",replacement="\\1neighbourhood")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])behavior(al|s?[^\\w\\d\\-\\/])",replacement="\\1behaviour\\2")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])(fav|hon)or(abl[ey]|ed|ing|s|)([^\\w\\d\\-\\/])",replacement="\\1\\2our\\3\\4")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])labor(ed|ing|s?\\W)",replacement="\\1labour\\2")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/\\|])(flav|hum)or(ed|ful|fully|ings?|less|lessly|s\\W)",replacement="\\1\\2our\\3")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/\\|])color(ed|ful|fully|ings?|less|lessly)?",replacement="\\1colour\\2") #changed
  txt <- gsub(txt,pattern="(^| [\\w\\D]*\\-)(col|flav|hum)ored",replacement="\\1\\2oured")    #?
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])mold(ed|ing|s?\\W)",replacement="\\1mould\\2")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/=])(calib|fib|goit|lust|mit|nit|reconnoit|sab|saltpet|spect|theat|tit)er(ed|ing)",replacement="\\1\\2r\\3")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])(calib|fib|goit|lust|mit|nit|reconnoit|sab|saltpet|spect|theat|tit)er(s?\\W)",replacement="\\1\\2re\\3")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/=])(cent|epicent|recent)er(ed|ing)",replacement="\\1\\2r\\3")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])centers",replacement="\\1centres")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])(centi|kilo|milli|-|\\d\\s)meter",replacement="\\1\\2metre")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])(centi|milli|deci|pico|hecto|\\b)liter(s?\\b)",replacement="\\1\\2litre\\3")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])(dema|peda|mono|syna)gog(s?\\W)",replacement="\\1\\2gogue\\3")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\\\/\\-])(ana|cata|dia|epi|homo|mono|pro)log([^\\w\\d\\-\\/])",replacement="\\1\\2logue\\3")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])(bev|jew|lev|mod|rev|trav)el(e[dr]|ing)",replacement="\\1\\2ell\\3")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])([a-z]{2,8}[^egl ])el(e[dr]|est|ing)([^\\w\\d\\-\\/])",replacement="\\1\\2ell\\3\\4")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])(counci|dia)l(e[dr]|ing|or)([^\\w\\d\\-\\/])",replacement="\\1\\2ll\\3\\4")
  txt <- gsub(txt,pattern="(^|[^\\w\\d\\-\\/])(app|enthr|riv|sign|tot)al(e[dr]|ing)",replacement="\\1\\2all\\3")
  txt <- gsub(txt,pattern="(\\s[\\w]*)ll(ful|ment)",replacement="\\1l\\2")
  txt <- gsub(txt,pattern="(nobelpr|pics)ise",replacement="\\1ize")
  txt
}

OxEnglish <- function(txt){
  txt <- unprotectwords(txt)
  txt <- protectwords(txt)
  txt <- Simple(txt)
  txt <- re_zwords(txt)
  txt <- unprotectwords(txt)
  txt
}

BritishEnglish <- function(txt){
  txt <- unprotectwords(txt)
  txt <- protectwords(txt)
  txt <- Simple(txt)
  txt <- zwords(txt)
  txt <- unprotectwords(txt)
  txt
}

Simpleplus <- function(txt){
  txt <- unprotectwords(txt)
  txt <- Simple(txt)
  txt <- zwords(txt)
  txt <- unprotectwords(txt)
  txt
}

zwords <- function(txt){
  txt <- gsub(txt,pattern="([^\\w\\d\\-][a-z]{3,12}[^s ])iz(e[drs]?|ations?|ing)(\\W)",replacement="\\1is\\2\\3")
  txt <- gsub(txt,pattern="(empha|synthe)siz(e|ing)",replacement="\\1sis\\2")
  txt
}

re_zwords <- function(txt){
  txt <- gsub(txt,pattern="([^\\w\\d\\-][a-z]{5,12})is(ations?)(\\W)",replacement="\\1iz\\2\\3")
  txt <- gsub(txt,pattern="([^\\w\\d\\-][a-z]{0,12}(?:[aeiou][^aeiou]|al))is(e[drs]?|ing)(\\W)",replacement="\\1iz\\2\\3")
  txt <- gsub(txt,pattern="(empha|synthe)sis(e|ing)",replacement="\\1siz\\2")
  txt
}

unprotectwords <- function(txt){
  txt <- gsub(txt,pattern="(\\w)xXx(\\w)",replacement="\\1\\2")
  txt
}

