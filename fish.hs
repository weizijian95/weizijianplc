import System.IO
data Var=VarI Int
        |VarO Int
        |VarT Int 
        |VarM Int
        deriving (Eq, Show)
data Event=SingleEvent Var 
          |Not Event
          |And Event Event
          |Or Event Event
data Delay=DelayLRRE Int Int Int
data Scope=Global Delay
          |After Event Delay
          |Before Event Delay
          |AfterUntil Event Event Delay
          |SIn Scope Scope
          |SAnd Scope Scope
          |SOr Scope Scope
          |SNot Scope
data Property=Universality|Absence|Existence deriving Show
data Level=Level Int
data Specification=Spec Scope Property Var Level 
data Com=LD Var
        |LDN Var
        |A Com Var
        |AN Com Var
        |O Com Var 
        |ON Com Var
data Ladder=Outputc Com Var 
           |Outputt Com Var Int
data Project=EmptyProject 
             |Append Ladder Project
-- 定义所有类型的show接口
instance Show Com where
    show (LD v1)   ="LD\t"++show v1
    show (LDN v1)   ="LDN\t"++show v1 
    show (A c1 v1)   =show c1++"\nA\t"++show v1 
    show (AN c1 v1)   =show c1++"\nAN\t"++show v1  
    show (O c1 v1)   =show c1++"\nO\t"++show v1
    show (ON c1 v1)   =show c1++"\nON\t"++show v1
instance Show Ladder where
    show (Outputc c1 v1)   =show c1++"\n"++"=\t"++show v1
    show (Outputt c1 v1 t1) = show c1++"\n"++"TON\t"++show v1++","++show t1
instance Show Project where
    show EmptyProject   = ""
    show (Append l1 p1) = show l1 ++"\n"++show p1
instance Show Event where 
  show (SingleEvent v)=show v
  show (Not e)="Not ("++show e++")"
  show (And e1 e2)="("++show e1++") And ("++show e2 ++")"
  show (Or e1 e2)="("++show e1++") Or ("++show e2 ++")"
instance Show Scope where
  show (Global d)="Global "++show d
  show (After e d)="After "++show e++" "++show d
  show (Before e d)="Before "++show e++" "++show d
  show (AfterUntil e1 e2 d)="After "++show e1++" until "++show e2++" "++show d
  show (SIn s1 s2)=show s1++" In ("++show s2++")"
  show (SAnd s1 s2)=show s1++" And ("++show s2++")"
  show (SOr s1 s2)=show s1++" Or ("++show s2++")"
  show (SNot s)=" Not ("++show s++")"
instance Show Delay where
  show (DelayLRRE t1 t2 t3)="delayL="++show t1++" delayR="++show t2++" delayRE="++show t3
instance Show Level where
  show (Level x)="level="++show x
instance Show Specification where
    show (Spec s p v l)   = show s++" "++show p++" "++show v++" "++show l
-- 引导输入复合区间       
scopeGuide:: IO String
scopeGuide= do{
  putStrLn "请选择规约控制区间的类型 (a)在单个区间成立 (b)在两个区间的交集成立 (c)在两个区间都成立 (d)在区间2中寻找满足区间1要求的区间成立";
  scopetype<-getLine;
  case scopetype of 
    "a"->do {putStrLn "请根据向导录入单个区间";
             putStrLn "请选择规约控制区间的左端点类型 (a)程序启动时 (b)程序启动后t1时间 (c)某事件发生时 (d)某事件发生后t1时间 ";
                              ans1<-getLine ;
                              putStrLn "请选择规约控制区间的右端点类型 (a)无右端点 (b)左端点发生后t2时间 (c)某事件发生时 (d)某事件发生后t3时间 (e)左端点发生后t2时间或某事件发生时 (f)左端点发生后t2时间或某事件发生后t3时间";
                              ans2<-getLine ;
                              let answer=ans1++ans2  in
                                 do {case answer of  
                                        "aa"->do {return "Global"}
                                        "ab"->do {putStrLn "请输入t2"; info_t2<-getLine;
                                          return ("Global delayR="++info_t2)}
                                        "ac"->do {putStrLn "请输入右端点事件"; info_r<-eventGuide;
                                          return ("Before "++info_r)}
                                        "ad"->do {putStrLn "请输入右端点事件"; info_r<-eventGuide; putStrLn "请输入t3"; info_t3<-getLine;
                                          return ("Before "++info_r++" delay RE="++info_t3)}  
                                        "ae"->do {putStrLn "请输入右端点事件"; info_r<-eventGuide; putStrLn "请输入t2"; info_t2<-getLine;
                                          return ("Before "++info_r++" delay R="++info_t2)}  
                                        "af"->do {putStrLn "请输入右端点事件"; info_r<-eventGuide; putStrLn "请输入t2"; info_t2<-getLine;putStrLn"请输入t3"; info_t3<-getLine;
                                          return ("Before "++info_r++" delay R="++info_t2++" delay RE="++info_t3)}
                                        
                                        "ba"->do {putStrLn "请输入t1"; info_t1<-getLine;return("Global,"++"delayL="++info_t1++" ")}
                                        "bb"->do {putStrLn "请输入t1"; info_t1<-getLine;putStrLn "请输入t2"; info_t2<-getLine;
                                          return ("Global "++"delayL="++info_t1++" delayR="++info_t2)}
                                        "bc"->do {putStrLn "请输入t1"; info_t1<-getLine;putStrLn "请输入右端点事件"; info_r<-eventGuide;
                                          return ("Before "++info_r++" "++"delayL="++info_t1)}
                                        "bd"->do {putStrLn "请输入t1"; info_t1<-getLine;putStrLn "请输入右端点事件"; info_r<-eventGuide; putStrLn "请输入t3"; info_t3<-getLine;
                                          return ("Before "++info_r++"delayL="++info_t1++" delay RE="++info_t3)}  
                                        "be"->do {putStrLn "请输入t1"; info_t1<-getLine;putStrLn "请输入右端点事件"; info_r<-eventGuide; putStrLn "请输入t2"; info_t2<-getLine;
                                          return ("Before "++info_r++" delayL="++info_t1++" delay R="++info_t2)}  
                                        "bf"->do {putStrLn "请输入t1"; info_t1<-getLine;putStrLn "请输入右端点事件"; info_r<-eventGuide; putStrLn "请输入t2"; info_t2<-getLine;putStrLn"请输入t3"; info_t3<-getLine;
                                          return ("Before "++info_r++" delayL="++info_t1++" delay R="++info_t2++" delay RE="++info_t3)}   
                                        
                                        "ca"->do {putStrLn "请输入左端点事件"; info_l<-eventGuide;return ("After "++info_l)}
                                        "cb"->do {putStrLn "请输入左端点事件"; info_l<-eventGuide;putStrLn "请输入t2"; info_t2<-getLine;
                                          return ("After "++info_l++" delayR="++info_t2)}
                                        "cc"->do {putStrLn "请输入左端点事件"; info_l<-eventGuide;putStrLn "请输入右端点事件"; info_r<-eventGuide;
                                          return ("After "++info_l++" Until "++info_r)}
                                        "cd"->do {putStrLn "请输入左端点事件"; info_l<-eventGuide;putStrLn "请输入右端点事件"; info_r<-eventGuide; putStrLn "请输入t3"; info_t3<-getLine;
                                          return ("After "++info_l++" Until "++info_r++" delay RE="++info_t3)}  
                                        "ce"->do {putStrLn "请输入左端点事件"; info_l<-eventGuide;putStrLn "请输入右端点事件"; info_r<-eventGuide; putStrLn "请输入t2"; info_t2<-getLine;
                                          return ("After "++info_l++" Until "++info_r++" delay R="++info_t2)}  
                                        "cf"->do {putStrLn "请输入左端点事件"; info_l<-eventGuide;putStrLn "请输入右端点事件"; info_r<-eventGuide; putStrLn "请输入t2"; info_t2<-getLine;putStrLn"请输入t3"; info_t3<-getLine;
                                          return ("After "++info_l++" Until "++info_r++" delay R="++info_t2++" delay RE="++info_t3)}
                                        
                                        "da"->do {putStrLn "请输入左端点事件"; info_l<-eventGuide;putStrLn "请输入t1"; info_t1<-getLine;return ("After "++info_l++" delayL="++info_t1)}
                                        "db"->do {putStrLn "请输入左端点事件"; info_l<-eventGuide;putStrLn "请输入t1"; info_t1<-getLine;putStrLn "请输入t2"; info_t2<-getLine;
                                          return ("After "++info_l++" delayL="++info_t1++" delayR="++info_t2)}
                                        "dc"->do {putStrLn "请输入左端点事件"; info_l<-eventGuide;putStrLn "请输入t1"; info_t1<-getLine;putStrLn "请输入右端点事件"; info_r<-eventGuide;
                                          return ("After "++info_l++" Until "++info_r++" delayL="++info_t1)}
                                        "dd"->do {putStrLn "请输入左端点事件"; info_l<-eventGuide;putStrLn "请输入t1"; info_t1<-getLine;putStrLn "请输入右端点事件"; info_r<-eventGuide; putStrLn "请输入t3"; info_t3<-getLine;
                                          return ("After "++info_l++" Until "++info_r++" delayL="++info_t1++" delay RE="++info_t3)}  
                                        "de"->do {putStrLn "请输入左端点事件"; info_l<-eventGuide;putStrLn "请输入t1"; info_t1<-getLine;putStrLn "请输入右端点事件"; info_r<-eventGuide; putStrLn "请输入t2"; info_t2<-getLine;
                                          return ("After "++info_l++" Until "++info_r++" delayL="++info_t1++" delay R="++info_t2)}  
                                        "df"->do {putStrLn "请输入左端点事件"; info_l<-eventGuide;putStrLn "请输入t1"; info_t1<-getLine;putStrLn "请输入右端点事件"; info_r<-eventGuide; putStrLn "请输入t2"; info_t2<-getLine;putStrLn"请输入t3"; info_t3<-getLine;
                                          return ("After "++info_l++" Until "++info_r++" delayL="++info_t1++" delay R="++info_t2++" delay RE="++info_t3)} 

                                        _->do {putStrLn "输入非法，请检查后重新录入"; scopeGuide }
                                     }
    }
    "b"->do {putStrLn "请根据向导输入区间1"; scope1<-scopeGuide;putStrLn $"当前区间为"++scope1++" and 区间2，请输入区间2";scope2<-scopeGuide; putStrLn $"录入完成，区间为："++scope1++" and "++scope2;return ("("++scope1++")"++" and "++"("++scope2++")")}
    "c"->do {putStrLn "请根据向导输入区间1"; scope1<-scopeGuide;putStrLn $"当前区间为"++scope1++" or 区间2，请输入区间2";scope2<-scopeGuide; putStrLn $"录入完成，区间为："++scope1++" or "++scope2;return ("("++scope1++")"++" or "++"("++scope2++")")}
    "d"->do {putStrLn "请根据向导输入区间1"; scope1<-scopeGuide;putStrLn $"当前区间为"++scope1++" in 区间2，请输入区间2";scope2<-scopeGuide; putStrLn $"录入完成，区间为："++scope1++" in "++scope2;return ("("++scope1++")"++" in "++"("++scope2++")")}
    _->do {putStrLn "输入错误，请检查后重新录入"; s<-scopeGuide;return s}
}
eventGuide::IO String
-- 引导输入复合命题的事件
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
-- 规约引导,输出规约文件
specGuide:: Handle->IO()
specGuide specfile= 
        do {          
                putStrLn "请根据向导完成规约描述";
                putStrLn "请输入新规约的控制对象名称，若所有规约已输入完成，请输入done";
                info_p<-getLine;
                case info_p of 
                     "done"-> do {putStrLn "规约文件spec.txt生成完毕"; return ()}   
                     _->do 
                             {scope<-scopeGuide;
                              putStrLn "请选择对规约控制对象的要求 (a)在控制区间内一直发生 (b)在控制区间内一直不发生 (c)仅在控制区间的左端点发生";
                              ans3<-getLine ;
                              putStrLn "请输入一个整数代表该规约的优先级：";
                              level<-getLine ;
                              let {info_prop=case ans3 of
                                                  "a"->"Universality"
                                                  "b"->"Absence"
                                                  "c"->"Existence" } in
                                hPutStrLn specfile (scope++","++info_prop++" "++info_p++" level="++level);specGuide specfile } 
                             }
-- 读出生成的规约文件中的规约
readSpecfile::Handle->[Specification]
readSpecfile filename=[]
-- 核心算法：将一组规约转化为程序
transSpecsToProject::[Specification]->Project
transSpecsToProject specs=Append (Outputc (AN (O (LD(VarI 0)) (VarO 0)) (VarI 1)) (VarO 0)) EmptyProject
-- 主函数
main :: IO ()
main = do specfile <- openFile "spec.txt" ReadWriteMode
          specGuide specfile
          projectfile <- openFile "IL.txt" WriteMode
          let p1=transSpecsToProject (readSpecfile specfile) in 
            do{hPutStrLn projectfile (show p1 )  ; putStrLn "代码文件IL.txt生成完成";          hClose specfile;
            hClose projectfile}