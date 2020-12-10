import System.IO
main :: IO ()
main = do specfile <- openFile "spec.txt" ReadWriteMode
          specGuide specfile
          hClose specfile
specGuide:: Handle->IO()
specGuide specfile= 
        do {
                putStrLn "请根据向导完成规约描述";
                putStrLn "请输入新规约的控制对象名称，若所有规约已输入完成，请输入done";
                info_p<-getLine;
                case info_p of 
                     "done"-> do {putStrLn "规约文件spec.txt生成完毕"; return ()}   
                     _->do 
                             {putStrLn "请选择规约控制区间的左端点类型 (a)程序启动时 (b)程序启动后t1时间 (c)某事件发生时 (d)某事件发生后t1时间 ";
                              ans1<-getLine ;
                              putStrLn "请选择规约控制区间的右端点类型 (a)无右端点 (b)左端点发生后t2时间 (c)某事件发生时 (d)某事件发生后t3时间 (e)左端点发生后t2时间或某事件发生时 (f)左端点发生后t2时间或某事件发生后t3时间";
                              ans2<-getLine ;
                              putStrLn "请选择对规约控制对象的要求 (a)在控制区间内一直发生 (b)在控制区间内一直不发生 (c)仅在控制区间的左端点发生";
                              ans3<-getLine ;
                              let {answer=ans1++ans2;info_prop=case ans3 of
                                                                    "a"->"Universality"
                                                                    "b"->"Absence"
                                                                    "c"->"Existence" } in
                                 do {case answer of  
                                        "aa"->do {hPutStrLn specfile ("Global,"++info_prop++" "++info_p)}
                                        "ab"->do {putStrLn "请输入t2"; info_t2<-getLine;
                                          hPutStrLn specfile ("Global delayR="++info_t2++","++info_prop++" "++info_p)}
                                        "ac"->do {putStrLn "请输入右端点事件"; info_r<-eventGuide;
                                          hPutStrLn specfile ("Before "++info_r++","++info_prop++" "++info_p)}
                                        "ad"->do {putStrLn "请输入右端点事件"; info_r<-eventGuide; putStrLn "请输入t3"; info_t3<-getLine;
                                          hPutStrLn specfile ("Before "++info_r++" delay RE="++info_t3++","++info_prop++" "++info_p)}  
                                        "ae"->do {putStrLn "请输入右端点事件"; info_r<-eventGuide; putStrLn "请输入t2"; info_t2<-getLine;
                                          hPutStrLn specfile ("Before "++info_r++" delay R="++info_t2++","++info_prop++" "++info_p)}  
                                        "af"->do {putStrLn "请输入右端点事件"; info_r<-eventGuide; putStrLn "请输入t2"; info_t2<-getLine;putStrLn"请输入t3"; info_t3<-getLine;
                                          hPutStrLn specfile ("Before "++info_r++" delay R="++info_t2++" delay RE="++info_t3++","++info_prop++" "++info_p)}
                                        
                                        "ba"->do {putStrLn "请输入t1"; info_t1<-getLine;hPutStrLn specfile ("Global,"++"delayL="++info_t1++info_prop++" "++info_p)}
                                        "bb"->do {putStrLn "请输入t1"; info_t1<-getLine;putStrLn "请输入t2"; info_t2<-getLine;
                                          hPutStrLn specfile ("Global "++"delayL="++info_t1++"delayR="++info_t2++","++info_prop++" "++info_p)}
                                        "bc"->do {putStrLn "请输入t1"; info_t1<-getLine;putStrLn "请输入右端点事件"; info_r<-eventGuide;
                                          hPutStrLn specfile ("Before "++info_r++" "++"delayL="++info_t1++","++info_prop++" "++info_p)}
                                        "bd"->do {putStrLn "请输入t1"; info_t1<-getLine;putStrLn "请输入右端点事件"; info_r<-eventGuide; putStrLn "请输入t3"; info_t3<-getLine;
                                          hPutStrLn specfile ("Before "++info_r++"delayL="++info_t1++" delay RE="++info_t3++","++info_prop++" "++info_p)}  
                                        "be"->do {putStrLn "请输入t1"; info_t1<-getLine;putStrLn "请输入右端点事件"; info_r<-eventGuide; putStrLn "请输入t2"; info_t2<-getLine;
                                          hPutStrLn specfile ("Before "++info_r++" delayL="++info_t1++" delay R="++info_t2++","++info_prop++" "++info_p)}  
                                        "bf"->do {putStrLn "请输入t1"; info_t1<-getLine;putStrLn "请输入右端点事件"; info_r<-eventGuide; putStrLn "请输入t2"; info_t2<-getLine;putStrLn"请输入t3"; info_t3<-getLine;
                                          hPutStrLn specfile ("Before "++info_r++" delayL="++info_t1++" delay R="++info_t2++" delay RE="++info_t3++","++info_prop++" "++info_p)}   
                                        
                                        "ca"->do {putStrLn "请输入左端点事件"; info_l<-eventGuide;hPutStrLn specfile ("After "++info_l++","++info_prop++" "++info_p)}
                                        "cb"->do {putStrLn "请输入左端点事件"; info_l<-eventGuide;putStrLn "请输入t2"; info_t2<-getLine;
                                          hPutStrLn specfile ("After "++info_l++" delayR="++info_t2++","++info_prop++" "++info_p)}
                                        "cc"->do {putStrLn "请输入左端点事件"; info_l<-eventGuide;putStrLn "请输入右端点事件"; info_r<-eventGuide;
                                          hPutStrLn specfile ("After "++info_l++" Until "++info_r++","++info_prop++" "++info_p)}
                                        "cd"->do {putStrLn "请输入左端点事件"; info_l<-eventGuide;putStrLn "请输入右端点事件"; info_r<-eventGuide; putStrLn "请输入t3"; info_t3<-getLine;
                                          hPutStrLn specfile ("After "++info_l++" Until "++info_r++" delay RE="++info_t3++","++info_prop++" "++info_p)}  
                                        "ce"->do {putStrLn "请输入左端点事件"; info_l<-eventGuide;putStrLn "请输入右端点事件"; info_r<-eventGuide; putStrLn "请输入t2"; info_t2<-getLine;
                                          hPutStrLn specfile ("After "++info_l++" Until "++info_r++" delay R="++info_t2++","++info_prop++" "++info_p)}  
                                        "cf"->do {putStrLn "请输入左端点事件"; info_l<-eventGuide;putStrLn "请输入右端点事件"; info_r<-eventGuide; putStrLn "请输入t2"; info_t2<-getLine;putStrLn"请输入t3"; info_t3<-getLine;
                                          hPutStrLn specfile ("After "++info_l++" Until "++info_r++" delay R="++info_t2++" delay RE="++info_t3++","++info_prop++" "++info_p)}
                                        
                                        "da"->do {putStrLn "请输入左端点事件"; info_l<-eventGuide;putStrLn "请输入t1"; info_t1<-getLine;hPutStrLn specfile ("After "++info_l++" delayL="++info_t1++","++info_prop++" "++info_p)}
                                        "db"->do {putStrLn "请输入左端点事件"; info_l<-eventGuide;putStrLn "请输入t1"; info_t1<-getLine;putStrLn "请输入t2"; info_t2<-getLine;
                                          hPutStrLn specfile ("After "++info_l++" delayL="++info_t1++" delayR="++info_t2++","++info_prop++" "++info_p)}
                                        "dc"->do {putStrLn "请输入左端点事件"; info_l<-eventGuide;putStrLn "请输入t1"; info_t1<-getLine;putStrLn "请输入右端点事件"; info_r<-eventGuide;
                                          hPutStrLn specfile ("After "++info_l++" Until "++info_r++" delayL="++info_t1++","++info_prop++" "++info_p)}
                                        "dd"->do {putStrLn "请输入左端点事件"; info_l<-eventGuide;putStrLn "请输入t1"; info_t1<-getLine;putStrLn "请输入右端点事件"; info_r<-eventGuide; putStrLn "请输入t3"; info_t3<-getLine;
                                          hPutStrLn specfile ("After "++info_l++" Until "++info_r++" delayL="++info_t1++" delay RE="++info_t3++","++info_prop++" "++info_p)}  
                                        "de"->do {putStrLn "请输入左端点事件"; info_l<-eventGuide;putStrLn "请输入t1"; info_t1<-getLine;putStrLn "请输入右端点事件"; info_r<-eventGuide; putStrLn "请输入t2"; info_t2<-getLine;
                                          hPutStrLn specfile ("After "++info_l++" Until "++info_r++" delayL="++info_t1++" delay R="++info_t2++","++info_prop++" "++info_p)}  
                                        "df"->do {putStrLn "请输入左端点事件"; info_l<-eventGuide;putStrLn "请输入t1"; info_t1<-getLine;putStrLn "请输入右端点事件"; info_r<-eventGuide; putStrLn "请输入t2"; info_t2<-getLine;putStrLn"请输入t3"; info_t3<-getLine;
                                          hPutStrLn specfile ("After "++info_l++" Until "++info_r++" delayL="++info_t1++" delay R="++info_t2++" delay RE="++info_t3++","++info_prop++" "++info_p)} 

                                        _->do {putStrLn "输入非法，请检查后重新录入"}                                     
                                ;specGuide specfile } 

                             }
        }
eventGuide::IO String
eventGuide=do {
        putStrLn "请选择事件类型 (a)发生某原子事件,即p (b)不发生某事件，即not e (c)某两事件同时发生，即e1 and e2 (d)某两事件发生其一，即e1 or e2 ";
        ans<-getLine;
        case ans of  
                "a"->do {putStrLn"请录入发生的事件名称"; p<-getLine; return p}
                "b"->do {putStrLn"请录入不发生的事件"; p<-eventGuide;return ("not ("++p++")")}
                "c"->do {putStrLn"请录入事件1"; p1<-eventGuide; putStrLn $"当前事件为："++p1++" and 事件2"; putStrLn"请录入事件2"; p2<-eventGuide;return ("("++p1++" and "++p2++")")}
                "d"->do {putStrLn"请录入事件1"; p1<-eventGuide; putStrLn $"当前事件为："++p1++" or 事件2"; putStrLn"请录入事件2"; p2<-eventGuide;return ("("++p1++" or "++p2++")")}
                _-> do {putStrLn"非法输入，请重新输入"; p<-eventGuide;return p}
}