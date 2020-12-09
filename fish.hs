import System.IO
import Data.Char(toUpper)
main :: IO ()
main = do specfile <- openFile "spec.txt" ReadWriteMode
          specGuide specfile
          hClose specfile
specGuide:: Handle->IO()
specGuide specfile= 
        do {
                putStrLn "请根据向导完成规约描述，若所有规约已输入完成，请输入done";
                putStrLn "(a)要求控制对象在整个区间内持续成立 (b)要求控制对象在整个区间内持续不成立 (c)要求控制对象在区间左端点成立，其余时刻均不成立 （done）规约输入完成";
                ans<-getLine;
                let info_prop= case ans of 
                                    "a"->"Universality"
                                    "b"->"Absence"
                                    "c"->"Existence"
                                    _->ans                                   
                in do {case ans of 
                   "done"->do {putStrLn "规约文件spec.txt生成完成" ;  return()} 
                   _ ->do {
                           putStrLn "1.选择对控制对象成立范围的要求（a）无条件 （b）在左参照点成立后 （c）在两个参照点之间 (d)与参照点同时成立";
                           ans1<-getLine;
                           case ans1 of
                                "a"-> do {
                                        putStrLn "请输入控制对象名称";
                                        info_p<-getLine;
                                        putStrLn "1_1.选择控制对象成立范围的起始点和终止点情况（a）起始点为系统初始时刻，无终止点 （b）起始点为 t1 时刻，无终止点 （c） 起始点为 t1 时刻，终止点为 t2 时刻";
                                        ans1_1<-getLine;
                                        case ans1_1 of 
                                             "a"->do {hPutStrLn specfile ("Global delayL=0，delayR=null "++info_prop ++" "++info_p);
                                              specGuide specfile}                               
                                             "b"->do {putStrLn "请输入t1值";
                                              info_t1<-getLine;
                                              hPutStrLn specfile ("Global delayL="++info_t1++" delayR=null "++info_prop++" "++info_p);
                                              specGuide specfile}
                                             "c"->do {putStrLn "请输入t1值";                                            
                                              info_t1<-getLine;
                                              putStrLn "请输入t2值";
                                              info_t2<-getLine;
                                              hPutStrLn specfile ("Global delayL="++info_t1++" delayR="++info_t2++" "++info_prop++" "++info_p);
                                              specGuide specfile   }
                                }
                                "b"-> do {
                                        putStrLn "请输入控制对象名称";
                                        info_p<-getLine;
                                        putStrLn "请输入左参照点名称";
                                        info_q<-getLine;
                                        putStrLn $ "1_2.选择控制对象成立范围的起始点和终止点情况（a）起始点为"++info_q++"，无终止点 （b）起始点为"++info_q++"后 t1 时刻，无终止点 （c） 起始点为"++info_q++"后 t1 时刻，终止点为"++info_q++"后 t2 时刻";
                                        ans1_2<-getLine;
                                        case ans1_2 of 
                                             "a"->do {hPutStrLn specfile ("After "++info_q++" delayL=0，delayR=null "++info_prop++" "++info_p);
                                             specGuide specfile}
                                             "b"->do {putStrLn "请输入t1值";
                                             info_t1<-getLine;
                                             hPutStrLn specfile ("After "++info_q++" delayL="++info_t1++" delayR=null "++info_prop++" "++info_p);
                                             specGuide specfile}
                                             "c"->do {putStrLn "请输入t1值";
                                             info_t1<-getLine;
                                             putStrLn "请输入t2值";
                                             info_t2<-getLine;
                                             hPutStrLn specfile ("After "++info_q++" delayL="++info_t1++" delayR="++info_t2++" "++info_prop++" "++info_p);
                                             specGuide specfile}
                                }
                                "c"->do {
                                        putStrLn "请输入控制对象名称";
                                        info_p<-getLine;
                                        putStrLn "请输入左参照点名称";
                                        info_q<-getLine;
                                        putStrLn "请输入右参照点名称";
                                        info_r<-getLine;
                                        putStrLn $ "1_3.选择控制对象成立范围的起始点和终止点情况（a）起始点为"++info_q++"，终止点为"++info_r++" （b）起始点为"++info_q++"后 t1 时刻，终止点为"++info_r++" （c）起始点为"++info_q++"，终止点为"++info_q++"后t2时刻或"++info_r++" （d）起始点为"++info_q++"，终止点为"++info_r++"后t3时刻（e） 起始点为"++info_q++"后 t1 时刻，终止点为"++info_q++"后 t2 时刻或"++info_r++" (f)起始点为"++info_q++"后 t1 时刻，终止点为"++info_r++"后t3时刻 ";
                                        ans1_3<-getLine;
                                        case ans1_3 of 
                                             "a"->do {hPutStrLn specfile ("After "++info_q++" Until "++info_r++"  delayL=0，delayR=null delayRE=0 "++info_prop++" "++info_p);
                                             specGuide specfile}
                                             "b"->do {putStrLn "请输入t1值";
                                             info_t1<-getLine;
                                             hPutStrLn specfile ("After "++info_q++" Until "++info_r++"  delayL="++info_t1++"，delayR=null delayRE=0 "++info_prop++" "++info_p);
                                             specGuide specfile}
                                             "c"->do {putStrLn "请输入t2值";
                                             info_t2<-getLine;
                                             hPutStrLn specfile ("After "++info_q++" Until "++info_r++"  delayL=0，delayR="++info_t2++" delayRE=0 "++info_prop++" "++info_p);
                                             specGuide specfile   }                 
                                             "d"->do {putStrLn "请输入t3值";
                                             info_t3<-getLine;
                                             hPutStrLn specfile ("After "++info_q++" Until "++info_r++"  delayL=0，delayR=null delayRE="++info_t3++" "++info_prop++" "++info_p);
                                             specGuide specfile}
                                             "e"->do {putStrLn "请输入t1值";
                                             info_t1<-getLine;
                                             putStrLn "请输入t2值";
                                             info_t2<-getLine;
                                             hPutStrLn specfile ("After "++info_q++" Until "++info_r++"  delayL="++info_t1++"，delayR="++info_t2++" delayRE=0 "++info_prop++" "++info_p);
                                             specGuide specfile}
                                             "f"->do {putStrLn "请输入t1值";
                                             info_t1<-getLine;
                                             putStrLn "请输入t3值";
                                             info_t3<-getLine;
                                             hPutStrLn specfile ("After "++info_q++" Until "++info_r++"  delayL="++info_t1++"，delayR=null delayRE="++info_t3++" "++info_prop++" "++info_p);
                                             specGuide specfile }
                                }
                                "d"->do {
                                        putStrLn "请输入控制对象名称";
                                        info_p<-getLine;
                                        putStrLn "请输入参照点名称";
                                        info_q<-getLine;
                                        putStrLn $ "1_4.选择控制对象成立范围的起始点和终止点情况（a）起始点为"++info_q++"，终止点为q不再成立的时刻 （b）起始点为"++info_q++"后 t1 时刻，终止点为q不再成立的时刻 （c）起始点为"++info_q++"，终止点为"++info_q++"后t2时刻或q不再成立的时刻 （d）起始点为"++info_q++"，终止点为q不再成立的时刻后t3时刻（e） 起始点为"++info_q++"后 t1 时刻，终止点为"++info_q++"后 t2 时刻或q不再成立的时刻 (f)起始点为"++info_q++"后 t1 时刻，终止点为q不再成立的时刻后t3时刻 ";
                                        ans1_4<-getLine;
                                        case ans1_4 of 
                                             "a"->do {hPutStrLn specfile ("When "++info_q++" delayL=0，delayR=null delayRE=0 "++info_prop++" "++info_p);
                                             specGuide specfile}
                                             "b"->do {putStrLn "请输入t1值";
                                             info_t1<-getLine;
                                             hPutStrLn specfile ("When "++info_q++"  delayL="++info_t1++"，delayR=null delayRE=0 "++info_prop++" "++info_p);
                                             specGuide specfile}
                                             "c"->do {putStrLn "请输入t2值";
                                             info_t2<-getLine;
                                             hPutStrLn specfile ("When "++info_q++"  delayL=0，delayR="++info_t2++" delayRE=0 "++info_prop++" "++info_p);
                                             specGuide specfile}
                                             "d"->do {putStrLn "请输入t3值";
                                             info_t3<-getLine;
                                             hPutStrLn specfile ("When "++info_q++"  delayL=0，delayR=null delayRE="++info_t3++" "++info_prop++" "++info_p);
                                             specGuide specfile}
                                             "e"->do {putStrLn "请输入t1值";
                                             info_t1<-getLine;
                                             putStrLn "请输入t2值";
                                             info_t2<-getLine;
                                             hPutStrLn specfile ("When "++info_q++"  delayL="++info_t1++"，delayR="++info_t2++" delayRE=0 "++info_prop++" "++info_p);
                                             specGuide specfile}
                                             "f"->do {putStrLn "请输入t1值";
                                             info_t1<-getLine;
                                             putStrLn "请输入t3值";
                                             info_t3<-getLine;
                                             hPutStrLn specfile ("When "++info_q++"  delayL="++info_t1++"，delayR=null delayRE="++info_t3++" "++info_prop++" "++info_p);
                                             specGuide specfile}
                                }
                   }
                }
        }