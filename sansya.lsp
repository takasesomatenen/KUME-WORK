;;----------------------------------
;;  SANSYA.LSP  三斜面積根拠計算作図
;;  (C) T.Sugimoto 2005～2018 All Right Reserved
;;  Ver.0.00 2005.09.21
;;  Ver.1.00 2006.01.16
;;  Ver.1.01 2006.01.17 変数追加
;;  Ver.1.02 2006.01.20 AREACALとの調整、チェックの強化
;;  Ver.1.03 2006.11.01 デフォルト画層の変更
;;  Ver.1.04 2006.12.13 表書込み文字位置調整
;;  Ver.1.05 2007.03.28 作表時エラー修正
;;  Ver.1.06 2007.06.08 "1"の書き込み修正
;;  Ver.1.07 2007.07.11 2回目の表書き込みは固定じゃないに修正
;;  Ver.1.08 2009.04.02 areasRtosのバグ
;;  Ver.1.09 2010.07.21 ソート確認修正
;;  Ver.1.10 2010.09.24 areasRtosのバグ
;;  Ver.1.11 2010.10.12 areasRtosのバグ
;;  Ver.1.12 2011.01.13 集計方法少し変更
;;  Ver.1.13 2017.11.08 小円弧分割調整
;;  Ver.1.14 2018.08.20 userr1
;----------------------------------
(prompt "SANSYA 三斜面積計算 by T.Sugimoto Ver.1.14 2018.8.20")
(setq cfgfname (strcat (substr (getvar "ACADPREFIX") 1 2) "/klib/klib.cfg"))
(cond
   ((findfile cfgfname)
      (setq f (open cfgfname "r"))
      (setq SANDIR (read-line f))      ;lsp & dcl  install dir
      (setq SANSUB (read-line f))      ;group file install dir
      (setq SANSTN (read-line f))      ;each station work  dir
      (setq SAN_EN (read-line f))      ; \ mark
      (close f)
   )
   (T (prompt "\n環境が整っていませんので起動できません")(exit))
)
;------------------------------------------------------------
(defun C:SANSYA( / sansyamh scl zudata zuname kidiaopen what )
   (if (= nil (tblsearch "BLOCK" "AMARK"))(mk_amark))
   (if (null (tblsearch "LAYER" "AREA1"))(_setlayer "AREA1" "CONTINUOUS" 3))   ;areahulay
   (if (null (tblsearch "LAYER" "AREA2"))(_setlayer "AREA2" "CONTINUOUS" 1))   ;areaselay
   (if (null (tblsearch "LAYER" "AREA3_TXT"))(_setlayer "AREA3_TXT" "CONTINUOUS" 3)) ;areamolay Ver.1.03
   (if (= nil $keynum)(setq $keynum "1"))
   (if (null (findfile (strcat SANSTN "sansya.ini")))
      (setq what (sansyadialog (sandefini)))
      (setq what (sansyadialog (readsanini)))
   )
   (if (= what 0)(exit))
   (if (null (findfile (strcat SANSTN "sansya.ini")))(exit))
   (setq mojiscl (/ 1.0 (getvar "cannoscalevalue")))       ;AreacalVer.2.63
   (setq usrr (getvar "USERR1"))
   (if (/= mojiscl usrr)
      (setvar "USERR1" mojiscl)
   )
   (setq scl (getvar "USERR1"))
   (if (= 0.0 (setq scl (getvar "USERR1")))(setq scl (getvar "DIMSCALE")))  ;図面縮尺倍率
   (if (= nil (tblsearch "BLOCK" "AMARK"))(mk_amark))
   (graphscr)
   (cond
      ((= what 1)
         (prompt "三斜面積計算を自動作図します。\n閉ポリラインの")
         (setq zudata (entget (ssname (ssget) 0)))
         (setq zuname (cdr (assoc 0 zudata)))
         (if (= zuname "LWPOLYLINE")(sansyamk zudata $keynum))
      )
      ((= what 2) (kobetukouji))
      ((= what 3) (syukei))
   )
   (princ)
)

(defun sansyadialog ( lst / retlst checksuuchi actok actdisp sansya_dlg what)
;marksize : "面積記号の直径φ"
;strwide  : "文字の大きさ"
;colhight : "セルの高さ　"
;colwide1 ; "番号セルの幅"
;colwide2 : "根拠セルの幅"
;colwide3 : "面積セルの幅"
;areasunp : "寸法記入" "sunpnasi" "sunpari"
;areamosize : "図寸法文字大きさ"
;areaclup  : "表示桁数"
;areakiri  : "表示桁数以下"  "kiri_age"  "kiri_shisya"  "kiri_sute"
;arcseg    : "円弧分割数"
;areakulay : "区分図形"
;areahulay : "面積符号"
;areamolay : "寸法文字"
;areaselay ; "表罫線字"
;hugonum   ; "記号開始番号";保存しない
;areatalay : "区分高さ画層"
;areahmlay ; "表文字画層"

   (defun checksuuchi(str / flg ) ;数値の文字列ならT、そうでない場合はnil
      (if (and (<= 45 (ascii str))(>= 57 (ascii str)))
         (if (= 'REAL  (type (atof str))) (setq flg T))
      )
      flg
   )   
   (defun actok( / check )
      (if (checksuuchi (get_tile "marksize"))
         (if (checksuuchi (get_tile "strwide"))
            (if (checksuuchi (get_tile "colhight"))
               (if (checksuuchi (get_tile "colwide1"))
                  (if (checksuuchi (get_tile "colwide2"))
                     (if (checksuuchi (get_tile "colwide3"))
                        (if (checksuuchi (get_tile "areamosize"))
                           (if (checksuuchi (get_tile "areaclup"))
                              (if (checksuuchi (get_tile "arcseg"))
                                 (setq check T)
                                 (alert "円弧分割長に数値を入力してください")
                              )
                              (alert "表示桁数に数値を入力してください")
                           )      
                           (alert "図形内寸法文字の大きさに数値を入力してください")
                        )
                        (alert "面積セルの巾に数値を入力してください")
                     )
                     (alert "根拠セルの巾に数値を入力してください")
                  )
                  (alert "番号セルの巾に数値を入力してください")
               )
               (alert "セルの高さに数値を入力してください")
            )
            (alert "文字の大きさに数値を入力してください")
         )
         (alert "面積記号の直径に数値を入力してください")
      )
      (if check
        (progn
         (if (get_tile "areakulay")
            (setq kulay (nth (atoi (get_tile "areakulay"))(cplaylst)))
            (setq kulay (car (cplaylst)))
         )
         (if (get_tile "areahulay")
            (setq hulay (nth (atoi (get_tile "areahulay"))(cplaylst)))
            (setq hulay (car (cplaylst)))
         )
         (if (get_tile "areamolay")
            (setq molay (nth (atoi (get_tile "areamolay"))(cplaylst)))
            (setq molay (car (cplaylst)))
         )
         (if (get_tile "areaselay")
            (setq selay (nth (atoi (get_tile "areaselay"))(cplaylst)))
            (setq selay (car (cplaylst)))
         )
         (if (get_tile "areatalay")
            (setq talay (nth (atoi (get_tile "areatalay"))(cplaylst)))
            (setq talay (car (cplaylst)))
         )
         (if (get_tile "areahmlay")
            (setq hmlay (nth (atoi (get_tile "areahmlay"))(cplaylst)))
            (setq hmlay (car (cplaylst)))
         )
         (setq retlst '())
         (setq retlst (append retlst (list (get_tile "marksize"))))
         (setq retlst (append retlst (list (get_tile "strwide"))))
         (setq retlst (append retlst (list (get_tile "colhight"))))
         (setq retlst (append retlst (list (get_tile "colwide1"))))
         (setq retlst (append retlst (list (get_tile "colwide2"))))
         (setq retlst (append retlst (list (get_tile "colwide3"))))
         (setq retlst (append retlst (list (get_tile "areasunp"))))
         (setq retlst (append retlst (list (get_tile "areamosize"))))
         (setq retlst (append retlst (list (get_tile "areaclup"))))
         (setq retlst (append retlst (list (get_tile "areakiri"))))
         (setq retlst (append retlst (list (get_tile "arcseg"))))
         (setq retlst (append retlst (list kulay)))
         (setq retlst (append retlst (list hulay)))
         (setq retlst (append retlst (list molay)))
         (setq retlst (append retlst (list selay)))
         (setq retlst (append retlst (list talay)))
         (setq retlst (append retlst (list hmlay)))
         (writesanini retlst)
         (setq $keynum (get_tile "hugonum"))
        )
     )
   )
   (defun actok1( / ) (actok) (done_dialog 1))     ;iniを書き込んで閉じる
   (defun actok2( / ) (actok) (done_dialog 2))     ;iniを書き込んで閉じる
   (defun actok3( / ) (actok) (done_dialog 3))     ;iniを書き込んで閉じる

   (defun actdef( / ) (actdisp (sandefini)) )

   (defun actdisp( lst / )
      (set_tile "marksize"  (nth 0 lst))
      (set_tile "strwide"   (nth 1 lst))
      (set_tile "colhight"  (nth 2 lst))
      (set_tile "colwide1"  (nth 3 lst))
      (set_tile "colwide2"  (nth 4 lst))
      (set_tile "colwide3"  (nth 5 lst))
      (if (= (nth 6 lst) "sunpari")
         (set_tile "areasunp" "sunpari")
         (set_tile "areasunp" "sunpnasi")
      )
      (set_tile "areamosize" (nth 7 lst))
      (set_tile "areaclup"   (nth 8 lst))
      (cond
         ((= (nth 9 lst) "kiri_age") (set_tile "areakiri" "kiri_age"))
         ((= (nth 9 lst) "kiri_shisya") (set_tile "areakiri" "kiri_shisya"))
         ((= (nth 9 lst) "kiri_sute") (set_tile "areakiri" "kiri_sute"))
      )
      (set_tile "arcseg"   (nth 10 lst))
      
      (start_list "areakulay")
      (mapcar 'add_list (cplaylst))
      (end_list)
      (if (setq buff (icchiban (cplaylst) (nth 11 lst)))
         (set_tile "areakulay" (itoa buff))
      )

      (start_list "areahulay")
      (mapcar 'add_list (cplaylst))
      (end_list)
      (if (setq buff (icchiban (cplaylst) (nth 12 lst)))
         (set_tile "areahulay" (itoa buff))
      )

      (start_list "areamolay")
      (mapcar 'add_list (cplaylst))
      (end_list)
      (if (setq buff (icchiban (cplaylst) (nth 13 lst)))
         (set_tile "areamolay" (itoa buff))
      )

      (start_list "areaselay")
      (mapcar 'add_list (cplaylst))
      (end_list)
      (if (setq buff (icchiban (cplaylst) (nth 14 lst)))
         (set_tile "areaselay" (itoa buff))
      )

      (start_list "areatalay")
      (mapcar 'add_list (cplaylst))
      (end_list)
      (if (setq buff (icchiban (cplaylst) (nth 15 lst)))
         (set_tile "areatalay" (itoa buff))
      )
      (start_list "areahmlay")
      (mapcar 'add_list (cplaylst))
      (end_list)
      (if (setq buff (icchiban (cplaylst) (nth 16 lst)))
         (set_tile "areahmlay" (itoa buff))
      )
      (set_tile "hugonum" $keynum)
   )
   (defun sansya_dlg( lst / )
      (if (not (new_dialog "sansyadia" dcl_id ))(exit))
      (actdisp lst )
      (action_tile "accept"    "(actok1)")  ;auto_button
      (action_tile "defvar"    "(actdef)")  ;デフォルトbutton
      (action_tile "sansya_B"  "(actok2)")  ;三斜
      (action_tile "syukei_B"  "(actok3)")  ;集計
      (action_tile "cancel"    "(done_dialog 0)")
      (start_dialog)
   )
;---------sansyadialog main--------------------------- 
   (setq dcl_id (load_dialog (strcat SANDIR "sansya.dcl")))
   (setq what (sansya_dlg lst))
   (unload_dialog dcl_id)
   what
)

(defun sansyamk(zudata keynum / verlst vislst ptyp pnum m h i j p
                                  taimin tai imin newver newvis
                                  oce blp osm clay orgkeynum)
   (command "_undo" "BE")
   (setq oce (getvar "CMDECHO"))
   (setq blp (getvar "BLIPMODE"))
   (setq osm (getvar "OSMODE"))
   (setq ort (getvar "ORTHOMODE"))
   (setvar "CMDECHO" 0)
   (setvar "BLIPMODE" 0)
   (setvar "OSMODE" 0)
   (setvar "ORTHOMODE" 0)
   (setq clay (getvar "CLAYER"))
;
   (setq zuname (cdr (assoc 0 zudata)))
   (setq ptyp   (cdr (assoc 70 zudata)))                  ;=1 閉
   (setq arcseg (atof (r-arcseg)))          ;円弧分割長さ
   (if (/= nil ptyp) (setq ptyp (logand ptyp 1)))         ;128 Ver.1.08
   (if (and (= zuname "LWPOLYLINE")(= ptyp 1))            ;close type pline only
     (progn
      (setq verlst (getlwpoarcver_autoseg zudata arcseg))
      (setq verlst (orderlst verlst))
      (setq hyoulst (sansyakouji  verlst clay ))
      (hyoukouji  hyoulst clay osm )
     )
   )
   (setvar "CLAYER" clay)
   (setvar "BLIPMODE" blp)
   (setvar "CMDECHO" oce)
   (setvar "OSMODE" osm)
   (setvar "ORTHOMODE" ort)
   (command "_undo" "E")
)

;三斜自動作図工事
(defun sansyakouji( verlst clay / 
                    pnum vislst m h i j taimin tai imin p newver newvis badlst alllst 
                    hyoulst ii bufflst)
   (setq pnum (length verlst))
   (setq hyoulst '())
   (setq bufflst (readsanini))
   (setq areasunp   (nth 6 bufflst))          ; "寸法記入" "sunpnasi" "sunpari"
   (setq areamosize (atof (nth 7 bufflst)))   ; "図寸法文字大きさ"
   (setq areaclup   (atoi (nth 8 bufflst)))   ; "表示桁数"
   (setq areakiri         (nth 9 bufflst)) ; "表示桁数以下"  "kiri_age"  "kiri_shisya"  "kiri_sute"
   (setq arcseg     (atoi (nth 10 bufflst)))  ; "円弧分割数"
   (setq areakulay  (nth 11 bufflst))         ; "区分図形"
   (setq areahulay  (nth 12 bufflst))         ; "面積符号"
   (setq areamolay  (nth 13 bufflst))         ; "寸法文字"
   (setq areaselay  (nth 14 bufflst))         ; "表罫線字"
   (setq areatalay  (nth 15 bufflst))         ; "区分高さ画層"
   (if (= 3 pnum)
     (progn
      (setq hyoulst (append hyoulst (list (sansyadraw (car verlst) (cadr verlst)(nth 2 verlst)))))
     ) 
     (progn
      (setq alllst verlst)
      (setq m pnum)
      (setq i 0 )
      (setq badlst '())
      (while (> m i)
         (if (= i 0)(setq h (- m 1))(setq h (- i 1)))
         (if (= i (- m 1))(setq j 0)(setq j (+ i 1)))
         (if (null (hantokei h i j verlst))
            (setq badlst (append badlst (list i)))
         )
         (setq i (1+ i))
      )
      (if (= 0 (length badlst))
        (progn                          ;凹がない場合
         (cond
            ((and (<= 4 pnum)(> 6 pnum))    ;一点から放射
               (setq ii 0)
               (repeat (- pnum 2)
                  (setq hyoulst (append hyoulst (list
                     (sansyadraw (car verlst) (nth (+ ii 1) verlst)(nth (+ ii 2) verlst))
                  )))
                  (setq ii (1+ ii))
               )
            )
            ((<= 6 pnum)                 ;台形っぽく分割
               (setq p 0)
               (setq q 2)
               (setq hyoulst (list (sansyadraw (nth p verlst) (nth 1 verlst)(nth q verlst))))
               (setq ii 1)
               (setq mm (/ (- pnum 3) 2))
               (repeat mm
                  (setq nexp (- pnum ii))
                  (setq nexq (+ ii 2))
                  (setq hyoulst (append hyoulst (list
                     (sansyadraw (nth nexp verlst) (nth p verlst)(nth q verlst))   )))
                  (setq hyoulst (append hyoulst (list
                     (sansyadraw (nth nexp verlst) (nth q verlst)(nth nexq verlst))   )))
                  (setq p nexp)
                  (setq q nexq)
                  (setq ii (1+ ii))
               )
               (if (= 0 (rem pnum 2))        ;残りの三角形
                 (progn
                  (setq hyoulst (append hyoulst (list
                     (sansyadraw (nth p verlst) (nth (+ q 1) verlst)(nth q verlst))
                  )))
                 )
               )
            )
         )
        )
        (progn                          ;凹ありの場合
         (setq pplst (minmin_lst verlst 60.0))
         (while (car pplst)
            (if (tennasi_flst (car (car pplst)) verlst alllst)
              (progn
               (setq pp (car (car pplst)))
               (setq pplst '())
              )
            )
            (setq pplst (cdr pplst))
         )
         (setq hyoulst (append hyoulst (list
            (sansyadraw_flst  pp verlst)  )))
         (setq verlst (san_dellst verlst pp))
         (setq m (length verlst))
         (while (< 3 m)
            (setq applst '())
            (setq mmm 1)
            (while (> 2 (length applst))
               (setq tempp (+ pp (* (expt -1 mmm)(/ mmm 2))))    ;0 1 -1 2 -2 3 -3 
               (if (tennasi_flst tempp verlst alllst)
                  (if (< (setq tempk (kakudo tempp verlst)) 180.0)
                     (setq applst (append applst (list (list tempp (abs (- tempk 60.0))))))
                  )
               )
               (setq mmm (1+ mmm))
            )
            (if (<= (cadr (car applst))(cadr (cadr applst)))    ;60度に近い点を選択
               (setq pp (car (car applst)))
               (setq pp (car (cadr applst)))
            )
            (setq hyoulst (append hyoulst (list
                   (sansyadraw_flst  pp verlst)  )))
            (setq verlst (san_dellst verlst pp))
            (setq m (length verlst))
         )
         (setq hyoulst (append hyoulst (list
            (sansyadraw_flst  pp verlst)  )))
        )
      )
     )
   )
   hyoulst
)

;三斜個別作図工事
(defun kobetukouji( / vislst m h i j taimin tai imin p newver newvis badlst alllst 
                      hyoulst ii bufflst  oce blp osm clay loop pt0 pt1 pt2)
   (command "_undo" "BE")
   (setq oce (getvar "CMDECHO"))
   (setq blp (getvar "BLIPMODE"))
   (setq osm (getvar "OSMODE"))
   (setq ort (getvar "ORTHOMODE"))
   (setvar "CMDECHO" 0)
   (setvar "BLIPMODE" 0)
   (setq clay (getvar "CLAYER"))
;
   (setq hyoulst '())
   (setq bufflst (readsanini))
   (setq areasunp   (nth 6 bufflst))          ; "寸法記入" "sunpnasi" "sunpari"
   (setq areamosize (atof (nth 7 bufflst)))   ; "図寸法文字大きさ"
   (setq areaclup   (atoi (nth 8 bufflst)))   ; "表示桁数"
   (setq areakiri         (nth 9 bufflst)) ; "表示桁数以下"  "kiri_age"  "kiri_shisya"  "kiri_sute"
   (setq arcseg     (atoi (nth 10 bufflst)))  ; "円弧分割数"
   (setq areakulay  (nth 11 bufflst))         ; "区分図形"
   (setq areahulay  (nth 12 bufflst))         ; "面積符号"
   (setq areamolay  (nth 13 bufflst))         ; "寸法文字"
   (setq areaselay  (nth 14 bufflst))         ; "表罫線字"
   (setq areatalay  (nth 15 bufflst))         ; "区分高さ画層"

   (setq loop T)
   (while loop
      (setq pt0 (getpoint "\n一点目を指示："))
      (if (/= pt0 nil)
        (progn
         (setq pt1 (getpoint pt0 "\n二点目を指示："))
         (if (/= pt1 nil)
           (progn
            (setq pt2 (getpoint pt1 "\n三点目を指示："))
            (if (/= pt2 nil)
              (progn
               (setvar "OSMODE" 0)
               (sansyadraw pt0 pt1 pt2)
               (setvar "OSMODE" osm)
              )
               (setq loop nil)
            )
           )
            (setq loop nil)
         )
        )
         (setq loop nil)
      )
   )
   (setvar "CLAYER" clay)
   (setvar "BLIPMODE" blp)
   (setvar "CMDECHO" oce)
   (setvar "OSMODE" osm)
   (setvar "ORTHOMODE" ort)
   (command "_undo" "E")
)

;集計工事
(defun syukei( / vislst m h i j taimin tai imin p newver newvis badlst alllst 
                 hyoulst ii bufflst  oce blp osm clay pt0 pt1 apenaa apenbb newlst buff klst 
                 aa bb cc sortedlst)
    (defun apenaa( klst / retstr)
       (setq retstr "")
       (while (/= (car klst) "×")
          (setq retstr (strcat retstr (car klst)))
          (setq klst (cdr klst))
       )
       retstr
    )
    (defun apenbb( klst / retstr)
       (while (/= (car klst) "×")
          (setq klst (cdr klst))
       )
       (setq klst (cdr klst))
       (setq retstr "")
       (while (/= (car klst) "÷")
          (setq retstr (strcat retstr (car klst)))
          (setq klst (cdr klst))
       )
       retstr
    )
    (defun chksuu( lst str / ret)      ;Ver.1.02
       (setq ret 0)
       (while (car lst)
          (if (= str (car lst))
             (setq ret (1+ ret))
          )
          (setq lst (cdr lst))
       )
       ret
    ) 
;------------------                
   (command "_undo" "BE")
   (setq oce (getvar "CMDECHO"))
   (setq blp (getvar "BLIPMODE"))
   (setq osm (getvar "OSMODE"))
   (setq ort (getvar "ORTHOMODE"))
   (setvar "CMDECHO" 0)
   (setvar "BLIPMODE" 0)
   (setq clay (getvar "CLAYER"))
;
   (setq hyoulst '())
   (setq bufflst (readsanini))
   (setq areasunp   (nth 6 bufflst))          ; "寸法記入" "sunpnasi" "sunpari"
   (setq areamosize (atof (nth 7 bufflst)))   ; "図寸法文字大きさ"
   (setq areaclup   (atoi (nth 8 bufflst)))   ; "表示桁数"
   (setq areakiri         (nth 9 bufflst)) ; "表示桁数以下"  "kiri_age"  "kiri_shisya"  "kiri_sute"
   (setq arcseg     (atoi (nth 10 bufflst)))  ; "円弧分割数"
   (setq areakulay  (nth 11 bufflst))         ; "区分図形"
   (setq areahulay  (nth 12 bufflst))         ; "面積符号"
   (setq areamolay  (nth 13 bufflst))         ; "寸法文字"
   (setq areaselay  (nth 14 bufflst))         ; "表罫線字"

   (prompt "面積データの集計をしますので面積記号を")    ;sansya Ver.1.12
   (setq ss (ssget))
;   (setq pt0 (getpoint "\n囲んだ範囲内の面積記号を集計します。一点目を："))
;   (setq pt1 (getcorner pt0 "二点目を:"))
;   (setq ss (ssget "W" pt0  pt1 ))
   (if (= ss nil)
      (prompt "\n面積記号が選択されていません")
     (progn
      (setq newlst '())
      (setq sslst (sanssread ss))
      (setq sortedlst (san_ss_sort sslst))
      (while (setq bufflst (car sortedlst))
         (setq buff (nth '2 bufflst))
         (setq klst (knj2lst buff))
         (setq knum (chksuu klst "×"))
         (if (and (= "2" (car (reverse klst)))(= "÷" (cadr (reverse klst)))(= knum 1))
           (progn
            (setq aa (apenaa klst))
            (setq bb (apenbb klst))
            (setq cc (car bufflst))
            (setq newlst (append newlst (list (list aa bb cc))))
           )
         )
         (setq sortedlst (cdr sortedlst))
      )
      (hyoukouji newlst clay osm)
     )
   )
   (setvar "CLAYER" clay)
   (setvar "BLIPMODE" blp)
   (setvar "CMDECHO" oce)
   (setvar "OSMODE" osm)
   (setvar "ORTHOMODE" ort)
   (command "_undo" "E")
)

;-----------------------------------------------------------------------
; 図形セットの記号種類読出
;ss     :図形セット
;リターン値:((符号 番号 箇所数)()()...)
(defun sanssread( ss / len i newlst zudata nailst)
   (setq len (sslength ss))
   (setq i 0)
   (setq newlst '())
   (repeat len
      (setq zudata (entget (ssname ss i)))
      (if (or (= "amark" (cdr (assoc 2 zudata)))
              (= "AMARK" (cdr (assoc 2 zudata))))
        (progn
         (setq nailst (readareanaiyou (ssname ss i)))
         (setq newlst (append newlst (list nailst)))
        )
      )
      (setq i (1+ i))
   )
   newlst
)
;-----------------------------------------------------------------------
;面積記号の内容を読み出す関数
;zudata    :エンティティ名
;リターン値:内容リスト(符号 番号 箇所数 左下 左上 右上)    str
(defun readareanaiyou( zudata / nextzu nel key mark area base room attribute newlst)
   (setq mark "")
   (setq area  "")
   (setq base "")
   (setq room "")
   (setq attribute "")
   (setq nextzu zudata)
   (while (/= "SEQEND" (cdr (assoc 0 (entget nextzu))))
      (setq nextzu (entnext nextzu))
      (setq nel (entget nextzu))
      (setq key (cdr (assoc 2 nel)))
      (cond
         ((= key "MARK")     (setq mark  (cdr (assoc 1 nel))))
         ((= key "AREA")     (setq area  (cdr (assoc 1 nel))))
         ((= key "BASE")     (setq base  (cdr (assoc 1 nel))))
         ((= key "ROOM")     (setq room  (cdr (assoc 1 nel))))
         ((= key "ATTRIBUTE")(setq attribute  (cdr (assoc 1 nel))))
      )
   )
   (setq newlst '())
   (setq newlst (append newlst (list mark)))
   (setq newlst (append newlst (list area)))
   (setq newlst (append newlst (list base)))
   (setq newlst (append newlst (list room)))
   (setq newlst (append newlst (list attribute)))
   newlst
)

(defun sansyadraw_flst (nn ptlst / mm p q r)
   (setq mm (length ptlst))
   (while (< nn 0)  (setq nn (+ nn mm)))
   (while (<= mm nn)(setq nn (- nn mm)))
   (cond
      ((= nn 0)
         (setq p (1- mm))
         (setq q nn)
         (setq r (1+ nn))
      )
      ((= nn (1- mm))
         (setq p (1- nn))
         (setq q nn)
         (setq r 0)
      )
      (T
         (setq p (1- nn))
         (setq q nn)
         (setq r (1+ nn))
      )
   )
   (sansyadraw (nth p ptlst)(nth q ptlst)(nth r ptlst))
)

(defun sansyadraw ( pt0 pt1 pt2 / wd0 wd1 wd2
                    l0 l1 l2 ll ang0 ppt0 ppt1 ppt2 kouten cyupt s-attreg s-attdia )
   (setq l0 (distance pt0 pt1))
   (setq l1 (distance pt1 pt2))
   (setq l2 (distance pt2 pt0))
   (setq ll (max l0 l1 l2))
   (cond
      ((= ll l0) (setq ppt0 pt0)(setq ppt1 pt1)(setq ppt2 pt2) )
      ((= ll l1) (setq ppt0 pt1)(setq ppt1 pt2)(setq ppt2 pt0) )
      ((= ll l2) (setq ppt0 pt2)(setq ppt1 pt0)(setq ppt2 pt1) )
   )
   (setq cyupt (list (/ (+ (car pt0)(car pt1)(car pt2)) 3.0)
                  (/ (+ (cadr pt0)(cadr pt1)(cadr pt2)) 3.0) ))
   (setq ang0 (angle ppt0 ppt1))
   (setq kouten (inters ppt0 ppt1 ppt2 (polar ppt2 (- ang0 (* 0.5 pi)) 1.0) nil))
   (setq ang1 (angle kouten ppt2))

   (if (= areakulay "<現在層>") (setvar "CLAYER" clay)(setvar "CLAYER" areakulay))
   (command "pline" ppt0 ppt1 ppt2 "c")
   (if (= areatalay "<現在層>") (setvar "CLAYER" clay)(setvar "CLAYER" areatalay))
   (command "line" kouten ppt2 "")
   (setq leng0 (distance ppt0 ppt1))
   (setq leng1 (distance kouten ppt2))
   (setq wd0 (areasRtos (* 0.001 leng0) 2 areaclup))       ;小数点桁数  mm to m
   (setq wd1 (areasRtos (* 0.001 leng1) 2 areaclup))       ;小数点桁数  mm to m
   
   (if (= areasunp "sunpari")
     (progn
      (if (= areamolay "<現在層>") (setvar "CLAYER" clay)(setvar "CLAYER" areamolay))
      (command "text" "j" "BC" (polar (polar ppt0 ang0 (* 0.5 leng0))
                            (+ ang0 (* 0.5 pi))(* scl areamosize 0.2)) 
                            (* scl areamosize)(/ (* 180.0 ang0) pi) wd0)
      (command "text" "j" "BC" (polar (polar kouten ang1 (* 0.5 leng1))
                            (+ ang1 (* 0.5 pi))(* scl areamosize 0.2)) 
                            (* scl areamosize)(/ (* 180.0 ang1) pi) wd1)
     )
   )

   (setq sanareastr (areasRtos (* (distof wd0 2) (distof wd1 2) 0.5) 2 areaclup))
   (setq basestr (strcat wd0 "×" wd1 "÷2"))

   (if (= areahulay "<現在層>")
      (setvar "CLAYER" clay)
      (setvar "CLAYER" areahulay)
   )
   (setq s-attreq (getvar "ATTREQ"))(setvar "ATTREQ" 1)
   (setq s-attdia (getvar "ATTDIA"))(setvar "ATTDIA" 0)
   (command "insert" "amark" cyupt (* scl (atof (nth 0 (readsanini))))  ""  "0.0" 
         sanareastr  basestr "" "" $keynum)
   (setvar "ATTREQ" s-attreq)
   (setvar "ATTDIA" s-attdia)
   (setq wd2 $keynum)
   (setq $keynum (incban $keynum))
   (list wd0 wd1 wd2)    ;底辺 高さ キー番号の文字列リストを返す
)

(defun incban( numstr / ascinc ret)
   (defun ascinc(asc / dic retasc)
      (setq dic (ascii asc))
      (cond
         ((and (<= 90 dic)(<= dic 96))(setq retasc (chr 97)))
         ((and (<= 122 dic)(<= dic 127))(setq retasc (chr 65)))
         (T (setq retasc (chr (1+ dic))))
      )
      retasc
   )
;-------------------------------   
   (if (> (atoi numstr) 0)
     (progn
      (setq ret (itoa (1+ (atoi numstr))))
     )
     (progn
      (if (= 1 (strlen numstr))
        (progn
         (setq ret (ascinc numstr))
        )
        (progn
         (setq hnum (substr numstr 1 1))
         (setq subnum (substr numstr 2))
         (if subnum
           (progn
            (setq subatno (atoi subnum))
            (if (> subatno 0)
               (setq ret (strcat hnum (itoa (+ (atoi subnum) 1))))
               (setq ret (strcat hnum (ascinc subnum)))
            )
           )
         )
        )
      )
     )
   )
   ret
)

;作表工事
(defun hyoukouji( hyoulst clay osm / ii mm  goukei osm keynum ptpt pt1)  ;Ver.1.07
   (defun cyoutext( jj pos ptt hh kaku wd / ftname)
     (progn
      (setq ftname (strcase (cdr (assoc 3 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))))
      (if (and (= "1" (substr wd (strlen wd) 1))
                  (= "SIMPLEX" ftname))                          ;Ver.1.04  Ver.1.06
         (command "text" jj pos  (pt ptt (* strwide -0.3) 0.0) hh kaku wd)
         (command "text" jj pos  ptt hh kaku wd)
      )
     )
   )
   (setq bufflst (readsanini))
   (setq strwide  (* scl (atof (nth 1 bufflst))))        ; "文字の大きさ"
   (setq colheight (* scl (atof (nth 2 bufflst))))       ; "セルの高さ"
   (setq colwide1 (* scl (atof (nth 3 bufflst))))        ; "番号セルの幅"
   (setq colwide2 (* scl (atof (nth 4 bufflst))))        ; "根拠セルの幅"
   (setq colwide3 (* scl (atof (nth 5 bufflst))))        ; "面積セルの幅"
   (setq areaclup  (atoi (nth 8 bufflst)))   ; "表示桁数"
   (setq areakiri        (nth 9 bufflst)) ; "表示桁数以下"  "kiri_age"  "kiri_shisya"  "kiri_sute"
   (setq areaselay  (nth 14 bufflst))        ; "表罫線画層"
   (setq areahmlay  (nth 16 bufflst))        ; "表文字画層"

   (setq mm (length hyoulst))
   (setq tatel (* (+ 5 mm) colheight))
   (setq yokol (+ colwide1 colwide2 colwide3))
   
   (setvar "OSMODE" osm)
;   (setq ptpt (sanhyoumove tatel yokol))
   (while (= ptpt nil) (setq ptpt (sanhyoumove tatel yokol)))    ;Ver.1.05
   (setvar "OSMODE" 0)
   
   (setq ii 1)
   (setq goukei 0.0)
   (setq pt1 ptpt)
   (setq mojib (* 0.5 (- colheight strwide)))
   (if (= areaselay "<現在層>")(setvar "CLAYER" clay)(setvar "CLAYER" areaselay))   ;罫線
   (command "line" pt1 (pt pt1 yokol 0.0) "")

   (setq linept (polar pt1 (* 1.5 pi) (* colheight ii)))
   (command "line" linept (pt linept yokol 0.0) "")
   (if (= areahmlay "<現在層>")(setvar "CLAYER" clay)(setvar "CLAYER" areahmlay))   ;文字
   (command "text" "J" "bc" (pt linept (* 0.5 colwide1) mojib) strwide 0.0 "記号")
   (command "text" "J" "bc" (pt linept (+ colwide1 (* 0.25 colwide2)) mojib) 
      strwide 0.0 "底辺（m）")
   (command "text" "J" "bc" (pt linept (+ colwide1 (* 0.75 colwide2)) mojib) 
      strwide 0.0 "高さ（m）")
   (command "text" "J" "bc" (pt linept (+ colwide1 colwide2 (* 0.5 colwide3)) mojib) 
      strwide 0.0 "面積（㎡）")
   (setq pt1 linept)
   (while (setq wdslst (car hyoulst))
      (setq linept (polar pt1 (* 1.5 pi) (* colheight ii)))
      (if (= areaselay "<現在層>")(setvar "CLAYER" clay)(setvar "CLAYER" areaselay))   ;罫線
      (command "line" linept (pt linept yokol 0.0) "")
      (if (= areahmlay "<現在層>")(setvar "CLAYER" clay)(setvar "CLAYER" areahmlay))   ;文字
      (cyoutext "J" "br" (pt linept (- colwide1 strwide) mojib) strwide 0.0 (nth 2 wdslst))
      (cyoutext "J" "br" (pt linept (+ colwide1 (- (* 0.5 colwide2) strwide)) mojib) 
         strwide 0.0 (car wdslst))
      (cyoutext "J" "br" (pt linept (+ colwide1 (- colwide2 strwide)) mojib) 
         strwide 0.0 (cadr wdslst))
      (setq sanareastr (areasRtos (* (distof (car wdslst) 2) (distof (cadr wdslst) 2)) 
         2 (* 2 areaclup)))
      (setq goukei (+ goukei (distof sanareastr 2)))
      (cyoutext "J" "br" (pt linept (+ colwide1 colwide2 (- colwide3 strwide)) mojib) 
         strwide 0.0 sanareastr)
      (setq hyoulst (cdr hyoulst))
      (setq ii (1+ ii))
   )
   (setq linept (polar pt1 (* 1.5 pi) (* colheight ii)))
   (setq goukeistr (areasRtos goukei 2 (* 2 areaclup)))
   (if (= areahmlay "<現在層>")(setvar "CLAYER" clay)(setvar "CLAYER" areahmlay))   ;文字
   (command "text" "J" "br" (pt linept (+ colwide1 (- colwide2 strwide)) mojib) 
      strwide 0.0 "計")
   (command "text" "J" "br" (pt linept (+ colwide1 colwide2 (- colwide3 strwide)) mojib) 
      strwide 0.0 goukeistr)
   (if (= areaselay "<現在層>")(setvar "CLAYER" clay)(setvar "CLAYER" areaselay))   ;罫線
   (command "line" linept (pt linept yokol 0.0) "")
   (setq ii (1+ ii))

   (setq linept (polar pt1 (* 1.5 pi) (* colheight ii)))
   (setq goukeistr2 (areasRtos (* 0.5 goukei) 2 (* 2 areaclup)))
   (if (= areahmlay "<現在層>")(setvar "CLAYER" clay)(setvar "CLAYER" areahmlay))   ;文字
   (command "text" "J" "br" (pt linept (+ colwide1 (- colwide2 strwide)) mojib) 
      strwide 0.0 "1/2")
   (command "text" "J" "br" (pt linept (+ colwide1 colwide2 (- colwide3 strwide)) mojib) 
      strwide 0.0 goukeistr2)
   (if (= areaselay "<現在層>")(setvar "CLAYER" clay)(setvar "CLAYER" areaselay))   ;罫線
   (command "line" linept (pt linept yokol 0.0) "")
   (setq ii (1+ ii))

   (setq linept (polar pt1 (* 1.5 pi) (* colheight ii)))
   (setq goukeistr3 (areasRtos (distof goukeistr2) 2 areaclup))
   (repeat areaclup (setq goukeistr3 (strcat goukeistr3 " ")))
   (if (= areahmlay "<現在層>")(setvar "CLAYER" clay)(setvar "CLAYER" areahmlay))   ;文字
   (command "text" "J" "br" (pt linept (+ colwide1 (- colwide2 strwide)) mojib) 
      strwide 0.0 "合計面積（㎡）")
;
;   (cyoutext "J" "br" (pt linept (+ colwide1 colwide2 (- colwide3 strwide)) mojib) 
;      strwide 0.0 goukeistr3)
;
   (command "text" "J" "br" (pt linept (+ colwide1 colwide2 (- colwide3 strwide)) mojib) 
      strwide 0.0 goukeistr3)

   (if (= areaselay "<現在層>")(setvar "CLAYER" clay)(setvar "CLAYER" areaselay))   ;罫線
   (command "line" linept (pt linept yokol 0.0) "")

   (command "line" ptpt (pt ptpt 0.0 (- (* (+ 4 mm) colheight))) "")
   (setq pt1 (pt ptpt colwide1 0.0))
   (command "line" pt1 (pt pt1 0.0 (- (* (+ 1 mm) colheight))) "")
   (setq pt1 (pt ptpt (+ colwide1 (* 0.5 colwide2)) 0.0 ))
   (command "line" pt1 (pt pt1 0.0 (- (* (+ 1 mm) colheight))) "")
   (setq pt1 (pt ptpt (+ colwide1 colwide2) 0.0 ))
   (command "line" pt1 (pt pt1 0.0 (- (* (+ 4 mm) colheight))) "")
   (setq pt1 (pt ptpt (+ colwide1 colwide2 colwide3) 0.0 ))
   (command "line" pt1 (pt pt1 0.0 (- (* (+ 4 mm) colheight))) "")
)

; 次点指示、範囲カーソル付き
(defun sanhyoumove( tatel yokol / tempdraw lastpt loop flg newpt)
   (defun tempdraw( cpt / ptt1 ptt2 ptt3)
      (setq ptt1 (pt cpt yokol 0.0))
      (setq ptt2 (pt cpt yokol (- tatel)))
      (setq ptt3 (pt cpt 0.0 (- tatel)))
      (setq drawlist '(-257))
      (setq drawlist (append drawlist (list cpt ptt1 ptt1 ptt2 ptt2 ptt3 ptt3 cpt)))
      (grvecs drawlist)
   )
 ;-----------------------------  
   (prompt "\n表の左上を指示：")
   (setq lastpt (list 0.0 0.0))
   (tempdraw lastpt)
   (setq loop T)
   (while loop
      (setq flg (car (setq lst (grread t 5 0))))
      (if (or (= 5 flg)(= 3 flg))(setq newpt (cadr lst)))
      (cond 
         ((= 5 flg)      ;ドラッキング
            (tempdraw lastpt)
            (tempdraw newpt)
            (setq lastpt newpt)
         )
         ((= 3 flg)      ;point set
            (tempdraw lastpt)
            (setq loop nil)
         )
         (t
            (tempdraw lastpt)
            (setq loop nil)
         )
      )
   )
   newpt
) 

(defun mk_amark( )
   (entmake '((0 . "BLOCK")(2 . "AMARK")(70 . 2)(10 0.0 0.0 0.0)))
   (entmake '((0 . "CIRCLE")(8 . "0")(10 0.0 0.0 0.0)(40 . 0.5)))
   (entmake '((0 . "ATTDEF")(8 . "0")(10 0.0 -0.2 0.0)(40 . 0.025)(1 . "")
                            (72 . 4)(11 0.0 -2.0 0.0)(41 . 1.0)
                            (3 . "AREA")(2 . "AREA")(70 . 1 )))
   (entmake '((0 . "ATTDEF")(8 . "0")(10 0.0 -0.27 0.0)(40 . 0.025)(1 . "")
                            (72 . 4)(11 0.0 -2.7 0.0)(41 . 1.0)
                            (3 . "BASE")(2 . "BASE")(70 . 1 )))
   (entmake '((0 . "ATTDEF")(8 . "0")(10 0.0 -3.4 0.0)(40 . 0.025)(1 . "")
                            (72 . 4)(11 0.0 -0.34 0.0)(41 . 1.0)
                            (3 . "ROOM NAME")(2 . "ROOM")(70 . 1 )))
   (entmake '((0 . "ATTDEF")(8 . "0")(10 0.0 -4.1 0.0)(40 . 0.025)(1 . "")
                            (72 . 4)(11 0.0 -0.41 0.0)(41 . 1.0)
                            (3 . "ROOM ATTRIBUTE")(2 . "ATTRIBUTE")(70 . 1 )))
   (entmake '((0 . "ATTDEF")(8 . "0")(10 0.0 0.0 0.0)(40 . 0.425)(1 . "")
                            (72 . 4)(11 0.0 0.0 0.0)(41 . 1.0)
                            (3 . "MARK")(2 . "MARK")(70 . 0 )))
   (entmake '((0 . "ENDBLK")))
   (princ)
)

;レイヤ設定*********************************************************by駒村*****
;   【プロトタイプ】
;      char _setlayer(char lay, char lt, int ct)
;   【必要とする関数】
;      なし
;   【引数】
;      lay : レイヤ名
;      lt  : 線種(CONTINUOUS:実線､DASHED:破線､CENTER:一点鎖線････)
;      ct  : 色種（１：赤、２：黄、３：緑、４：水色、５：青、６：紫、７：白）
;   【ローカル変数】
;      nowlayer : 現在のレイヤ
;      laylist  : 指定レイヤのリスト
;      flg1     : 指定レイヤの保管／非保管
;      flg2     : 指定レイヤのＯＮ／ＯＦＦ
;   【グローバル変数】
;      なし
;   【戻り値】
;      変更前のレイヤ
;   【機能説明】
;      現在レイヤを引数で与えられたレイヤに変更する為の関数。
;      もし与えられたレイヤが存在しない場合は、レイヤを作成します。
;   【関連関数】
;      なし
;****************************************************************************
(defun _setlayer(lay lt ct / nowlayer laylist flg1 flg2)
   (setq nowlayer (getvar "CLAYER"))
   (setq lay      (if lay lay nowlayer))
   (setq lt       (if lt  lt  "CONTINUOUS"))
   (setq ct       (if ct  ct  7))
   (setq laylist  (tblsearch "layer" lay))
   (if (= laylist nil)
      (command "LAYER" "M" lay "L" lt lay "C" ct lay "")
     (progn
      (setq flg1 (cdr (assoc '70 laylist)))
      (setq flg2 (cdr (assoc '62 laylist)))
      (command "LAYER")
      (if (/= flg1 64) (command "T"  lay))
      (if (<= flg2  0) (command "ON" lay))
      (command "S" lay "")
     )
   )
   nowlayer
)

;ライト ウェイト ポリラインの頂点リストを作る関数 from ABC p.267
;bug fix, append hukurami and lwpolyline対応  by T.Sugimoto   2005.09.20
; 更に円弧分割数の決定方法を変更  by T.Sugimoto 2006.01.16
(defun getlwpoarcver_autoseg( zudata arclen / zudata ptyp verlst vertex nextver cen hukurami
                                      verhulst kiten rr angdx sang ndis kakudo arcseg)
   (setq ptyp  (cdr (assoc 70 zudata)))                   ;=1 閉
   (if (/= nil ptyp) (setq ptyp (logand ptyp 1)))         ;128 Ver.1.08
   (setq verhulst '())
   (setq vertex nil)
   (setq hukurami nil)
   (while (setq templst (car zudata))
      (if (= '10 (car (assoc 10 (list templst))))
         (setq vertex (cdr (assoc 10 (list templst))))
      )
      (if (= '42 (car (assoc 42 (list templst))))
         (setq hukurami (cdr (assoc 42 (list templst))))
      )
      (if (and vertex hukurami)
        (progn
         (setq verhulst (append verhulst (list vertex hukurami)))
         (setq vertex nil)
         (setq hukurami nil)
        )
      )
      (setq zudata (cdr zudata))
   )
   (setq verlst '())
   (setq kiten (car verhulst))
   (while (setq vertex (car verhulst))
      (setq verlst (append verlst (list vertex)))
      (setq verhulst (cdr verhulst))
      (setq hukurami (car verhulst))
      (setq verhulst (cdr verhulst))
      (if (/= 0.0 hukurami)
        (progn
         (if verhulst
            (setq nextver (car verhulst))
            (if (= ptyp 1)
               (setq nextver kiten)
               (setq nextver vertex)
            )
         )
         (setq ndis (/ (distance vertex nextver) 2))
         (if (/= 0.0 ndis)
           (progn
            (setq kakudo (- (* 2 (atan hukurami))(/  pi 2.0)))
            (setq cen (polar
               (polar vertex (angle vertex nextver) ndis )
               (- (angle vertex nextver) (/ pi 2.0))
               (* ndis (/ (sin kakudo)(cos kakudo) ))
            ))
            (setq rr (distance vertex cen))

            (setq tempang (* 4 (atan hukurami)))
            (setq tempnum (abs (/ (* tempang rr) arclen)))
            (if (< tempnum 3.0)                           ;Ver.1.13
               (setq arcseg 3)
               (setq arcseg (fix tempnum))
            )
;            (if (< tempnum 8.0)
;               (setq arcseg 8)
;               (setq arcseg (fix tempnum))
;            )

            (setq angdx (/ (* 4 (atan hukurami)) arcseg))
            (setq sang (angle cen vertex))
            (repeat (1- arcseg)
               (setq sang (+ sang angdx))
               (setq verlst (append verlst (list (polar cen sang rr)) ))
            )
           )
         )  
        )
      )
   )
   verlst
)

;頂点リストを反時計回りに変換する関数
(defun orderlst(verlst / spl ppl cpl tverlst aarea darea)
   (setq spl (car verlst))
   (setq ppl spl)
   (setq tverlst (cdr verlst))
   (setq aarea 0)
   (while (setq cpl (car tverlst))
      (setq darea (* (- (car cpl)(car ppl))
                     (/ (+ (cadr cpl)(cadr ppl)) 2) ) )
      (setq aarea (+ aarea darea))
      (setq ppl cpl)
      (setq tverlst (cdr tverlst))
   )
   (setq darea (* (- (car spl)(car ppl))
                  (/ (+ (cadr spl)(cadr ppl)) 2) ) )
   (setq aarea (+ aarea darea))
   (if (> aarea 0.0)
      (reverse verlst)  ; 面積が正の時は時計回りなので反転
      verlst
   )
)

;反時計回りT、時計回りnil
(defun hantokei(h i j verlst / )
   (hantok (nth h verlst) (nth i verlst)(nth j verlst))
)

;反時計回りT、時計回りnil  その２ 入力が違う
(defun hantok(hp ip jp / xh xi xj yh yi yj x_hi x_hj y_hi y_hj determ)
   (setq xh (car hp)  xi (car ip)  xj (car jp))
   (setq yh (cadr hp) yi (cadr ip) yj (cadr jp))
   (setq x_hi (- xi xh))  (setq x_hj (- xj xh))
   (setq y_hi (- yi yh))  (setq y_hj (- yj yh))
   (setq determ (- (* x_hi y_hj)(* x_hj y_hi)))
   (> determ 0.0000000001)
)

(defun tennasi_flst( nn ptlst alllst / mm p q r)
   (setq mm (length ptlst))
   (while (< nn 0)
      (setq nn (+ nn mm))
   )
   (while  (<= mm nn)
      (setq nn (- nn mm))
   )
   (cond
      ((= nn 0)
         (setq p (1- mm))
         (setq q nn)
         (setq r (1+ nn))
      )
      ((= nn (1- mm))
         (setq p (1- nn))
         (setq q nn)
         (setq r 0)
      )
      (T
         (setq p (1- nn))
         (setq q nn)
         (setq r (1+ nn))
      )
   )
   (tennasi p q r ptlst alllst)
)
;他の頂点を内部に持たない時T 持っている時nil    2006.01.10 by TS
(defun tennasi(h i j verlst alllst / k naibu len kp kflg hflg iflg jflg loop hp ip jp)
   (setq k 0)
   (setq kflg 0)
   (setq naibu T)
   (setq len (length alllst))
   (setq hp (nth h verlst))
   (setq ip (nth i verlst))
   (setq jp (nth j verlst))
   (setq loop T)
   (while loop
      (setq kp (nth k alllst))
      (if (null (or (equal hp kp)(equal ip kp)(equal jp kp)))
        (progn
         (setq kflg 0)
         (setq hflg (inters hp ip kp (polar kp '0.0 '1.0E+10)))
         (setq iflg (inters ip jp kp (polar kp '0.0 '1.0E+10)))
         (setq jflg (inters jp hp kp (polar kp '0.0 '1.0E+10)))
         (if hflg 
            (if (< (car kp)(car hflg))
               (setq kflg (1+ kflg))
               (setq hflg nil)
            )
         )
         (if iflg 
            (if (< (car kp)(car iflg))
               (setq kflg (1+ kflg))
               (setq iflg nil)
            )
         )
         (if jflg 
            (if (< (car kp)(car jflg))
               (setq kflg (1+ kflg))
               (setq jflg nil)
            )
         )
         (if (= (rem kflg 2) 1)
           (progn
            (setq naibu nil)
            (setq loop nil)
           )
         )
        )
      )
      (setq k (1+ k))
      (if (<= len k)(setq loop nil))
   )
   naibu
)
;[237へのレス] Re: 内外判定 投稿者：にゃーこ 投稿日：2000/10/12(Thu) 12:45:57
;
;凹図形の判定:
;　すべての頂点の角度が１８０度以下かどうかで判定
;
;区画の包含関係の判定：
;　双方の辺が交差していない場合。
;　　　（頂点が他の区画の辺上にあるときの判定が必要）
;　　含まれる/含まれない：　そのうちの１点が区画内にあるかどうかで判定
;
;三斜求積の三角形分割：
;　１.鋭角（１８０度未満）の頂点を１つ指示（６０度に一番近い頂点）
;　２.最初の三角形を作成
;　３.多角形の頂点リストからその頂点を除く
;　４.作成した三角形の他の２頂点のうち６０度に近い頂点で三角形を作成
;　５.３と４を繰り返す（残りの頂点数が３になるまで）
;
;　４の時両端の角度が１８０度以上のときは、そのまた隣の頂点で考える
;
;※この三角形分割は、三斜求積だけじゃなく任意形状の重心を求める場合も有効っす。


;>交点が奇数回、現れれば内側。偶数回なら外側。
;この手法です。
;どっかＣのライブラリがあってそれをＶＢに置き換えて
;やってます。
;
;＞注意する点は無限線と区画線が重なったりしたときや、区画線交点を
;＞通ったときの処理かな。

;iの数とbadlst内の数とが一つでも一致したらnil, 全部一致しなければT
(defun alldef(i badlst / tlst flg)
   (setq tlst badlst)
   (setq flg T)
   (while tlst
      (if (= i (car tlst))(setq flg nil))
      (setq tlst (cdr tlst))
   )
   flg
)
; 頂点リストを与え、指定頂点の角度を360度までの角度値で返す、
(defun kakudo( ptnumber verlst / ptlen pt0 pt1 pt2 kakuseiki)
   (defun kakuseiki( a1 a2 / aa)
      (setq aa (* (/ 180.0 pi)(- a2 a1)))
      (while (< aa 0.0)
          (setq aa (+ aa 360.0))
      )
      (while  (<= 360.0 aa)
         (setq aa (- aa 360.0))
      )
      aa
   )
   (setq ptlen (length verlst))
   (while (< ptnumber 0)
      (setq ptnumber (+ ptlen ptnumber))
   )
   (while  (<= ptlen ptnumber)
      (setq ptnumber (- ptnumber ptlen))
   )
   (cond
      ((= ptnumber 0)
         (setq pt0 (nth (1- ptlen) verlst))
         (setq pt1 (nth ptnumber verlst))
         (setq pt2 (nth (1+ ptnumber) verlst))
      )
      ((= ptnumber (1- ptlen))
         (setq pt0 (nth (1- ptnumber) verlst))
         (setq pt1 (nth ptnumber verlst))
         (setq pt2 (nth 0 verlst))
      )
      (T
         (setq pt0 (nth (1- ptnumber) verlst))
         (setq pt1 (nth ptnumber verlst))
         (setq pt2 (nth (1+ ptnumber) verlst))
      )
   )
   (kakuseiki (angle pt1 pt2)(angle pt1 pt0))
)
; 頂点リストを与え、指定角との差をリスト化
(defun kakulst( verlst ang / ii ret)   ;頂点角度を求め、angとの差を度でリストにして返す
   (setq ii 0)
   (repeat (length verlst)
      (setq ret (append ret (list (abs (- (kakudo ii verlst) ang)))))
      (setq ii (1+ ii))
   )
   ret
)
; 指定の角度の近い頂点の番号を返す。
(defun minmin( kakulst / retval ret ii)
   (setq retval '1.0E+30)
   (setq ret 0)
   (setq ii 0)
   (repeat (length kakulst)
      (if (< (nth ii kakulst) retval)
        (progn
         (setq retval (nth ii kakulst))
         (setq ret ii)
        ) 
      )
      (setq ii (1+ ii))
   )
   ret
)
; 指定の角度の近い頂点の番号のリストを返す。
(defun minmin_lst( verlst ang / retval retlst ii ban_kakulst)  ;頂点角度を求め、angとの差を度でリストにしソート
   (setq ii 0)
   (repeat (length verlst)
      (setq ban_kakulst (append ban_kakulst (list (list ii (abs (- (kakudo ii verlst) ang))))))
      (setq ii (1+ ii))
   )
   (setq retlst (san_sort ban_kakulst))
)
; complst を定義後 _sortを呼出すこと
;         (defun complst(aa bb / flg )   ;(< aa bb)のboolを返
;            (cond
;               ((= nil aa)(setq flg T))
;               ((= nil bb)(setq flg nil))
;               ((and (= nil aa)(= nil bb))(setq flg nil))
;               (T  (setq flg (< (car aa)(car bb)) ))
;            )
;            flg
;         ) 
;         (setq outlst (_sort inplst))
(defun san_sort (inplst / low hi restlst temp bufflst lowlst hilst)
   (defun complst(aa bb / flg )   ;(< aa bb)のboolを返
      (cond
         ((= nil aa)(setq flg T))
         ((= nil bb)(setq flg nil))
         ((and (= nil aa)(= nil bb))(setq flg nil))
         (T  (setq flg (<= (cadr aa)(cadr bb)) ))         ;sansya Ver.1.09
      )
      flg
   ) 
   (setq bufflst inplst)
   (repeat (/ (length bufflst) 2)
      (setq low (car bufflst))
      (setq hi   nil)
      (setq restlst nil)
      (foreach x bufflst
         (if (complst x low) (setq temp low low x x temp) )
         (if (complst hi x)  (setq temp hi hi x x temp)   )
         (if x (setq restlst (cons x restlst))            )
      )
      (setq lowlst (append lowlst (list low)))
      (setq hilst  (append hilst  (if hi (list hi))))
      (setq bufflst (cdr(reverse restlst)))
   )
   (append lowlst bufflst (reverse hilst))
)

(defun san_ss_sort (inplst / low hi restlst temp bufflst lowlst hilst ms asi to64 complst)
   (defun ms(str / retflg numb )  ;文字種判定
      (if str 
        (progn
         (setq numb (ascii str))
         (cond
            ((and (<= 48 numb)(<= numb 57)) (setq retflg "dg")) ;先頭が数字
            ((and (<= 65 numb)(<= numb 90)) (setq retflg "mo")) ;先頭が大文字
            ((and (<= 97 numb)(<= numb 122))(setq retflg "mo")) ;先頭が小文字
            (T (setq retflg nil))
         )
        )
         (setq retflg nil)
      )
      retflg
   )
   (defun asi(str keta / retd kisuu) ;ascii変換の大文字小文字の順序を変え、桁乗する
      (setq kisuu 256)  ; 数値部分の最大数になってしまうので64を変更
      (if str 
        (progn
         (setq numb (ascii str))
         (cond
            ((and (<= 65 numb)(<= numb 90)) (setq retd (- (ascii str) 35))) ;先頭が大文字
            ((and (<= 97 numb)(<= numb 122))(setq retd (- (ascii str) 94))) ;先頭が小文字
            (T (setq retd 0))
         )
        )
         (setq retd 0)
      )
      (repeat keta
         (setq retd (* retd kisuu))
      )
      retd
   )
   (defun to64(str / retdig )            ;64進数に変換
      (cond
         ((= 1 (strlen str))
            (cond
               ((= "dg" (ms str))(setq retdig (atoi str)))
               ((= "mo" (ms str))(setq retdig (asi str 1)))
               (T (setq retdig 0))
            )
         )
         ((= 2 (strlen str))
            (cond
               ((= "dg" (ms str))(setq retdig (atoi str)))
               ((= "mo" (ms str))
                  (setq str2 (substr str 2))
                  (cond
                     ((= "dg" (ms str2))
                        (setq retdig (+ (asi str 1)(atoi str2)))
                     )
                     ((= "mo" (ms str2))
                        (setq retdig (+ (asi str 2)(asi str2 1)))
                     )
                     (T (setq retdig 0))
                  )
               )
               (T (setq retdig 0))
            )
         )
         ((<= 3 (strlen str))
            (cond
               ((= "dg" (ms str))(setq retdig (atoi str)))
               ((= "mo" (ms str))
                  (setq str2 (substr str 2))
                  (cond
                     ((= "dg" (ms str2))
                        (setq retdig (+ (asi str 1)(atoi str2)))
                     )
                     ((= "mo" (ms str2))
                        (setq str3 (substr str 3))
                        (cond
                           ((= "dg" (ms str3))
                              (setq retdig (+ (asi str 2)(asi str2 1)(atoi str3)))
                           )
                           ((= "mo" (ms str3))
                              (setq retdig (+ (asi str 3)(asi str2 2)(asi str3 1)))
                           )
                           (T (setq retdig 0))
                        )
                     )
                     (T (setq retdig 0))
                  )
               )
               (T (setq retdig 0))
            )
         )
         (T (setq retdig 0))
      )
      retdig
   )
   (defun complst(aa bb / flg )   ;(< aa bb)のboolを返
      (cond
         ((= nil aa)(setq flg T))
         ((= nil bb)(setq flg nil))
         ((and (= nil aa)(= nil bb))(setq flg nil))
         (T  (setq flg (<= (to64 aa)(to64 bb)) ))   ;Ver.2.51 =追加 Ver.1.09
      )
      flg
   )
   (defun complst_e(aa bb / flg bufflst_aa bufflst_bb)   ;(< aa bb)のboolを返   前処理 ;Ver.2.39
      (setq bufflst_aa (str2lst (car aa) "-"))
      (setq bufflst_bb (str2lst (car bb) "-"))
      (if (and (= 2 (length bufflst_aa))(= 2 (length bufflst_bb)))
         (if (= (car bufflst_aa)(car bufflst_bb))
            (setq flg (complst (cadr bufflst_aa)(cadr bufflst_bb)))
            (setq flg (complst (car bufflst_aa)(car bufflst_bb)))
         )
         (if (= (car bufflst_aa)(car bufflst_bb))
            (cond
               ((and  (= 2 (length bufflst_aa))(= 1 (length bufflst_bb)))
                  (setq flg nil)
               )
               ((and  (= 1 (length bufflst_aa))(= 2 (length bufflst_bb)))
                  (setq flg T)
               )
               (T 
                  (setq flg (complst (car bufflst_aa)(car bufflst_bb)))
;                  (setq flg nil)      ;Ver.2.51   Ver.1.09
               )
            )
            (setq flg (complst (car bufflst_aa)(car bufflst_bb)))
         ) 
      )
      flg
   )
;-------------------------sort main-----------------    
   (setq bufflst inplst)
   (repeat (/ (length bufflst) 2)
      (setq low (car bufflst))
      (setq hi   nil)
      (setq restlst nil)
      (foreach x bufflst
         (if (complst_e x low) (setq temp low low x x temp) )
         (if (complst_e hi x)  (setq temp hi hi x x temp)   )
         (if x (setq restlst (cons x restlst))            )
      )
      (setq lowlst (append lowlst (list low)))
      (setq hilst  (append hilst  (if hi (list hi))))
      (setq bufflst (cdr(reverse restlst)))
   )
   (append lowlst bufflst (reverse hilst))
)

(defun san_dellst(lst nn / ii retlst mm ) ;汎用リスト処理関数  nn番目を消す 負、オーバー対策済
   (setq retlst '())
   (setq mm (length lst))
   (while (<  nn 0) (setq nn (+ nn mm)))
   (while (<= mm nn)(setq nn (- nn mm)))
   (setq ii 0)
   (repeat (length lst)
      (if (/= ii nn) (setq retlst (append  retlst (list (nth ii lst)))))
      (setq ii (1+ ii))
   )
   retlst
)
;漢字入り文字列を分解して一文字づつのリストに   by T&M Res.
(defun knj2lst(kwd / bt1 bt2 bt3 bt5 k len ii retlst len)
   (if (= kwd nil)(setq kwd ""))
   (setq len (strlen kwd))
   (setq kwd (strcat kwd "    "))
   (setq ii 1)
   (setq retlst '())
   (while (<= ii len)
      (setq bt1 (substr kwd ii 1))
      (setq bt2 (substr kwd ii 2))
      (cond
         ((/= bt2 "%%")
            (if (or (< (ascii bt1) 129)(and (> (ascii bt1) 159)(< (ascii bt1) 224)))
               (setq retlst (append retlst (list bt1)) ii (+ ii 1))
               (setq retlst (append retlst (list bt2)) ii (+ ii 2))
            )
         )
         ((= bt2 "%%")
            (setq bt3 (substr kwd ii 3))
            (setq bt5 (substr kwd ii 5))
            (setq k (strcase(substr bt3 3 1)))
            (if (or (= k "O")(= k "U")(= k "D")(= k "P")(= k "C")(= k "%"))
               (setq retlst (append retlst (list bt3)) ii (+ ii 3))
               (setq retlst (append retlst (list bt5)) ii (+ ii 5))
            )
         )
      )
   )
   retlst
)
;------------------------------------------------------------------------------
; 文字列をリストに変換する関数
;  data_str : 文字列
;  kugiri   : 区切りとなる文字   " "が区切りの場合""をリストから外す
;  戻り値   : 変換されたリスト
(defun str2lst (data_str kugiri / data_lst str index ch data)
   (if (/= nil data_str)
     (progn
      (setq data_lst '()  str ""  index 1)
      (repeat (strlen data_str)
         (setq ch (substr data_str index 1))
         (if (= ch kugiri)
            (setq data_lst (cons str data_lst)   str "")
            (setq str (strcat str ch))
         )
         (setq index (1+ index))
      )
      (setq data_lst (reverse (cons str data_lst)))
      (if (= kugiri " ")
        (progn
         (setq data data_lst)
         (setq data_lst '())
         (while (setq str (car data))
            (if (/= str "")
              (progn
               (setq data_lst (cons str data_lst))
              )
            )
            (setq data (cdr data))
         )
         (setq data_lst (reverse data_lst))
        )
      )
     )
   )
   data_lst
)
;------------------------------------------------------------------------
;; ini file
;marksize : "面積記号の直径φ"
;strwide  : "文字の大きさ"
;colhight : "セルの高さ　"
;colwide1 ; "番号セルの幅"
;colwide2 : "根拠セルの幅"
;colwide3 : "面積セルの幅"
;areasunp : "寸法記入" "sunpnasi" "sunpari"
;areamosize : "図寸法文字大きさ"
;areaclup  : "表示桁数"
;areakiri  : "表示桁数以下"  "kiri_age"  "kiri_shisya"  "kiri_sute"
;arcseg    : "円弧分割数"
;areakulay : "区分図形"
;areahulay : "面積符号"
;areamolay : "寸法文字"
;areaselay ; "表罫線画層";
;hugonum   ; "記号開始番号";保存しない
;
;areatalay : "区分高さ画層"
;areahmlay ; "表文字画層"
(defun r-areasunp( / )   (nth 6 (readsanini)))
(defun r-areamosize( / ) (nth 7 (readsanini)))
(defun r-areaclup( / )   (nth 8 (readsanini)))
(defun r-areakiri( / )   (nth 9 (readsanini)))
(defun r-arcseg( / )     (nth 10 (readsanini)))
(defun sandefini( / )
   '("7.0" "3.0" "6.0" "10.0" "80.0" "40.0" "sunpari" "2.5" "3" "kiri_shisya" "1000"
     "AREA2" "AREA1" "AREA3_TXT" "AREA2" "AREA2" "AREA3_TXT" )   ;Ver.1.03
)
;
;ini読み出し
(defun readsanini( / retlst f)
   (setq retlst '())
   (if (findfile (strcat SANSTN "sansya.ini"))
     (progn
      (if (setq f (open (strcat SANSTN "sansya.ini") "r"))
        (progn
         (while (setq buff (read-line f))
            (setq retlst (append retlst (list buff)))
         )
         (close f)
        )
      )
     )
   )
   retlst
)
;ini書き出し
(defun writesanini( sanlst / f)
   (setq f (open (strcat SANSTN "sansya.ini") "w"))
   (while (car sanlst)
      (write-line (car sanlst) f)
      (setq sanlst (cdr sanlst))
   )
   (close f)
)
;まだまだあったバグ AREACALVer.2.52から 四捨五入にも問題2段式に変更
(defun areasRtos(real jp keta / tempstr retstr addketa dz matu karistr)   ;Ver.2.33a追加、rtosをすべて書換
   (setq dz (getvar "DIMZIN"))
   (setvar "DIMZIN" 1)   ;Ver.2.50重大なバグ修正 これが 0 だった。
   (cond
      ((= areakiri "kiri_sute")     ;切り捨て   MENSEKI Ver2.17
         (if (< 11 keta)(setq keta 11))
         (setq str (rtos real jp (+ keta 2)))
         (if (< 0 keta)
            (setq retstr (substr str 1 (- (strlen str) 2)))
            (setq retstr (substr str 1 (- (strlen str) 3)))
         )
      )
      ((= areakiri "kiri_age")     ;切り上げ       Ver.2.43  0は0だろ
         (setq karistr (rtos real jp (1+ keta)))
         (setq matu (substr karistr (strlen karistr) 1))   ;Ver.2.47 重大バグ 切出し位置失敗
         (if (= matu "0")
           (progn
            (if (< 0 keta)                      ;MENSEKI Ver2.17
               (setq retstr (substr karistr 1 (- (strlen karistr) 1)))
               (setq retstr (substr karistr 1 (- (strlen karistr) 2)))
            )
           )
           (progn 
            (setq retstr (substr karistr 1 (- (strlen karistr) 1)))
            (setq retstr (rtos (+ (atof retstr)(expt 0.1 keta)) jp keta))
           )
         )
      )
      ((= areakiri "kiri_shisya")     ;四捨五入
         (setq str (rtos real jp (+ keta 2)))     ;Ver.2.52
         (setq tt (substr str (- (strlen str) 1) 1))
         (if (= tt "5")                           ;5は強制的に切り上げに
           (progn
            (setq retstr (substr str 1 (- (strlen str) 2)))
            (setq retstr (rtos (+ (atof retstr)(expt 0.1 keta)) jp keta))
           )
           (progn
            (setq retstr (rtos real jp keta))
           )
         )
      )
      (T                              ;四捨五入
         (setq retstr (rtos real jp keta))
      )
   )
   (setvar "DIMZIN" dz)
   retstr
)

;まだあったバグ AREACAL Ver.2.47から
(defun areasRtos_old2(real jp keta / tempstr retstr addketa dz matu karistr)   ;Ver.2.33a追加、rtosをすべて書換
   (setq dz (getvar "DIMZIN"))
   (setvar "DIMZIN" 0)
   (cond
      ((= areakiri "kiri_sute")     ;切り捨て
;         (setq tempstr (rtos real jp (+ 2 keta)))
         (setq tempstr (rtos real jp (+ 10 keta)))    ;Ver.2.47 
;         (if (= keta 0)
;            (setq retstr (substr tempstr 1 (- (strlen tempstr) 3)))
;            (setq retstr (substr tempstr 1 (- (strlen tempstr) 2)))
;         )
         (if (= keta 0)
            (setq retstr (substr tempstr 1 (- (strlen tempstr) 11)))
            (setq retstr (substr tempstr 1 (- (strlen tempstr) 10)))
         )
      )
      ((= areakiri "kiri_age")     ;切り上げ       Ver.2.43  0は0だろ
         (setq karistr (rtos real jp (1+ keta)))
;         (setq matu (substr karistr (- (strlen karistr) 1) 1))
         (setq matu (substr karistr (strlen karistr) 1))   ;Ver.2.47 重大バグ 切出し位置失敗
         (if (= matu "0")
           (progn
            (setq retstr (substr karistr 1 (- (strlen karistr) 1)))
           )
           (progn 
            (setq retstr (substr karistr 1 (- (strlen karistr) 1)))
            (setq retstr (rtos (+ (atof retstr)(expt 0.1 keta)) jp keta))
           )
         )
      )
      ((= areakiri "kiri_shisya")     ;四捨五入
         (setq retstr (rtos real jp keta))
      )
      (T                              ;四捨五入
         (setq retstr (rtos real jp keta))
      )
   )
   (setvar "DIMZIN" dz)
   retstr
)
;areacal.lsp から複写 2006.01.13  areacal ver.2.33  変更sansya ver1.01 関数名から
(defun areasRtos_old(real jp keta / tempstr retstr addketa dz)   ;Ver.2.33a追加、rtosをすべて書換
   (setq dz (getvar "DIMZIN"))
   (setvar "DIMZIN" 0)
   (cond
      ((= areakiri "kiri_sute")     ;切り捨て
         (setq tempstr (rtos real jp (+ 2 keta)))
         (if (= keta 0)
            (setq retstr (substr tempstr 1 (- (strlen tempstr) 3)))
            (setq retstr (substr tempstr 1 (- (strlen tempstr) 2)))
         )
      )
      ((= areakiri "kiri_age")     ;切り上げ
         (setq addketa 0.499999999)
         (repeat keta (setq addketa (* addketa 0.1)))
         (setq retstr (rtos (+ real addketa) jp keta))
      )
      ((= areakiri "kiri_shisya")     ;四捨五入
         (setq retstr (rtos real jp keta))
      )
      (T                              ;四捨五入
         (setq retstr (rtos real jp keta))
      )
   )
   (setvar "DIMZIN" dz)
   retstr
)
(defun icchiban( lst str / ret ii)
   (setq ii 0)
   (setq ret nil)
   (while (setq buff (car lst))
      (if (= buff str) (setq ret ii) )
      (setq ii (1+ ii))
      (setq lst (cdr lst))
   )
   ret
)
(defun cplaylst( / )
   (append '( "<現在層>" ) (laylst))
)
(defun laylst( / retlst tbldata tblname)
   (setq retlst '())
   (setq tbldata (tblnext "LAYER" T))
   (setq tblname (cdr (assoc 2 tbldata)))
   (setq retlst (append retlst (list tblname)))
   (while (setq tbldata (tblnext "LAYER"))
      (setq tblname (cdr (assoc 2 tbldata)))
      (setq retlst (append retlst (list tblname)))
   )
   (acad_strlsort retlst)   ;sort
)
(defun stylelst( / retlst tbldata tblname)
   (setq retlst '())
   (setq tbldata (tblnext "STYLE" T))
   (setq tblname (cdr (assoc 2 tbldata)))
   (setq retlst (append retlst (list tblname)))
   (while (setq tbldata (tblnext "STYLE"))
      (setq tblname (cdr (assoc 2 tbldata)))
      (setq retlst (append retlst (list tblname)))
   )
   (acad_strlsort retlst)   ;sort
)
(defun pt (pp xx yy) (list (+ (car pp) xx) (+ (cadr pp) yy))) ;位置リスト
(princ)
;;-----End of SANSYA.LSP--------------------------------------------------
