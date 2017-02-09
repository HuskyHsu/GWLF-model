    PROGRAM GWLF11
    IMPLICIT NONE
    CHARACTER*30,ALLOCATABLE::NAME(:),NAMET(:)
    CHARACTER*30 NAME_T
    INTEGER,ALLOCATABLE::N_S(:),N_F(:),M(:),PAR(:),Y(:)
    INTEGER CHECK,N,NO,PAR_T,START,FINAL,I,J
    REAL,ALLOCATABLE::P(:,:),P5(:,:),T(:,:),WEIGHT(:),CN2(:),O(:,:),SS(:,:)
    REAL,ALLOCATABLE::Q(:,:),ET(:,:),GT(:,:),UT(:,:),C(:,:),PC(:,:),ST(:,:)
    REAL W_T,CN2_T,GT0,UT0,UR,ST0,AREA,R,CN3

    OPEN(1,FILE='INPUT.TXT')
    OPEN(5,FILE='TEMP.TXT')
    CHECK=0
    N=0
    READ(1,*)
    DO WHILE(CHECK==0)
     READ(1,*,IOSTAT=CHECK) NAME_T,W_T,CN2_T,PAR_T
     WRITE(5,'(A30,A6)') ADJUSTR(NAME_T),'-P.TXT'
     WRITE(5,'(A30,A6)') ADJUSTR(NAME_T),'-T.TXT'
     N=N+1
    ENDDO
    N=N-1
    REWIND(1)
    REWIND(5)
    ALLOCATE(NAME(N),NAMET(N),N_S(N),N_F(N),WEIGHT(N),CN2(N))
    ALLOCATE(PAR(N))

    READ(1,*) AREA
    DO I=1,N
     READ(1,*) NAME_T,WEIGHT(I),CN2(I),PAR(I)
     READ(5,*) NAME(I)
     READ(5,*) NAMET(I)
    ENDDO
    CLOSE(5)

    WRITE(*,'(1X,A30)') ADJUSTL('START CALLING SUBROUTINE')
    CALL CHECKFILE(NAME,NO,N,START,FINAL)
    WRITE(*,'(1X,A30)') ADJUSTL('FINSHED SUB-CHECKFILE')
    ALLOCATE(P(N,NO),P5(N,NO),T(N,NO),C(N,NO),Q(N,NO),M(NO),ET(N,NO),PC(N,NO),ST(N,NO+1))
    ALLOCATE(GT(N,NO),UT(N,NO+1),Y(NO),O(N,NO),SS(N,NO))

    CALL DAY5(NAME,N,NO,P,P5,START,FINAL)
    WRITE(*,'(1X,A30)') ADJUSTL('FINSHED SUB-DAY5')

    WRITE(*,'(1X,A30)') ADJUSTL('OPEN INITIAL.TXT')
    OPEN(5,FILE='INITIAL.TXT')
    READ(5,*) GT0
    WRITE(*,*) '初始地下水出流量為：',GT0
    READ(5,*) UT0
    WRITE(*,*) '初始未飽和層含水量為：',UT0
    READ(5,*) UR
    WRITE(*,*) '根層深度最大含水量為：',UR
    READ(5,*) ST0
    WRITE(*,*) '初始飽和層含水量為：',ST0
    READ(5,*) R
    WRITE(*,*) '退水係數為：',R
    CLOSE(5)
    WRITE(*,'(1X,A30)') ADJUSTL('CLOSE INITIAL.TXT')

    CALL INPUT_T(NAMET,N,NO,T,START,FINAL)
    WRITE(*,'(1X,A30)') ADJUSTL('FINSHED SUB-INPUT_T')

    CALL CN(NAME,C,CN2,N,NO,START,FINAL,P5,M,Y,CN3)
    WRITE(*,'(1X,A30)') ADJUSTL('FINSHED SUB-CN')

    CALL RUNOFF_ET_ESTIMATE_PC_UT(N,NO,C,P,Q,Y,T,M,ET,PAR,UT0,UR,UT,GT,ST0,R,PC,ST,CN3,O,SS)
    WRITE(*,'(1X,A30)') ADJUSTL('FINSHED SUB-RUNOFF_ET_ESTIMATE_PC_UT')

    CALL SF_OUTPUT(N,NO,Q,GT,WEIGHT,START,FINAL,AREA,Y,M,O)
    WRITE(*,'(1X,A30)') ADJUSTL('FINSHED SUB-SF_OUTPUT')
!~
    OPEN(13,FILE='ET.TXT')
    OPEN(14,FILE='GT.TXT')
    OPEN(15,FILE='P.TXT')
    OPEN(16,FILE='P5.TXT')
    OPEN(17,FILE='T.TXT')
    OPEN(18,FILE='UT.TXT')
    OPEN(19,FILE='QS.TXT')
    OPEN(20,FILE='PC.TXT')
    OPEN(21,FILE='ST.TXT')
    OPEN(22,FILE='CN.TXT')
    OPEN(23,FILE='O.TXT')
!~        OPEN(25,FILE='date_P_Q_G2.TXT')
!~     !OPEN(24,FILE='SS.TXT')

    DO J=1,NO
     WRITE(13,'(I10,6F10.2)') Y(J)*10000+M(J),(ET(I,J),I=1,N)
     WRITE(14,'(I10,6F10.2)') Y(J)*10000+M(J),(GT(I,J),I=1,N)
     WRITE(15,'(I10,6F10.2)') Y(J)*10000+M(J),(P(I,J),I=1,N)
     WRITE(16,'(I10,6F10.2)') Y(J)*10000+M(J),(P5(I,J),I=1,N)
     WRITE(17,'(I10,6F10.2)') Y(J)*10000+M(J),(T(I,J),I=1,N)
     WRITE(18,'(I10,6F10.2)') Y(J)*10000+M(J),(UT(I,J),I=1,N)
     WRITE(19,'(I10,6F10.2)') Y(J)*10000+M(J),(Q(I,J),I=1,N)
     WRITE(20,'(I10,6F10.2)') Y(J)*10000+M(J),(PC(I,J),I=1,N)
     WRITE(21,'(I10,6F10.2)') Y(J)*10000+M(J),(ST(I,J),I=1,N)
     WRITE(22,'(I10,6F10.2)') Y(J)*10000+M(J),(C(I,J),I=1,N)
     WRITE(23,'(I10,6F10.2)') Y(J)*10000+M(J),(O(I,J),I=1,N)
!~       WRITE(25,'(I10,6F10.2)') Y(J)*10000+M(J),(O(I,J),P(I,J),Q(I,J),GT(I,J),I=1,N)
!~      !WRITE(24,'(I10,6F10.2)') Y(J)*10000+M(J),(SS(I,J),I=1,N)
    ENDDO
!~     CLOSE(12)
!~     CLOSE(13)
    CLOSE(14)
!~     CLOSE(15)
!~     CLOSE(16)
!~     CLOSE(17)
!~     CLOSE(18)
    CLOSE(19)
!~     CLOSE(20)
!~     CLOSE(21)
!~     CLOSE(22)

!~     WRITE(*,*) '按ENTER離開'
!~     READ(*,*)

    stop
    end PROGRAM GWLF11

!---------------------------------------------------------------
    SUBROUTINE CHECKFILE(NAME,NO,N,START,FINAL)
    IMPLICIT NONE
    INTEGER N
    CHARACTER*30 NAME(N),DECISION
    INTEGER I,NO,CHECK,DATE_T,N_T,START,FINAL,N_S(N),N_F(N)
    REAL RAIN_T

    DO I=1,N
     OPEN(10+I,FILE=NAME(I))
     CHECK=0
     N_T=0
     DO WHILE(CHECK==0)
      IF(N_T>0)THEN
       N_F(I)=DATE_T  !讀到最後一個數時將日期記錄為N_F
      ENDIF
      READ(10+I,*,IOSTAT=CHECK) DATE_T,RAIN_T
      N_T=N_T+1  !讀入第一個數即將日期記錄為N_S
       IF(N_T==1)THEN
        N_S(I)=DATE_T
       ENDIF
     ENDDO
     CLOSE(10+I)

!決定初始日期和結束日期
     IF(I==1)THEN
      START=N_S(I)
      FINAL=N_F(I)
      ELSE
      IF(START<N_S(I))THEN
        START=N_S(I)
      ENDIF

      IF(FINAL>N_F(I))THEN
        FINAL=N_F(I)
      ENDIF
     ENDIF
    ENDDO
    CLOSE(5,STATUS='DELETE')
!~
     WRITE(*,'(1X,2(A6,I8,2x))') 'START=',START,'FINAL=',FINAL
!~ 27   WRITE(*,*) 'DO YOU WANT TO CHANG START AND FINAL?'
!~      WRITE(*,*) 'DO YOU WANT TO CHANG START AND FINAL?'
!~      WRITE(*,*) '修改按Y，不改按S'
!~      read(*,*) decision
!~      READ(*,*) DECISION
!~      IF(DECISION.EQ.'Y')THEN
!~ 28    WRITE(*,*) 'INPUT START YOU NEED...'
!~       WRITE(*,*) 'INPUT START YOU NEED...'
!~       READ(*,*) START
!~       WRITE(*,*) 'INPUT FINAL YOU NEED...'
!~       READ(*,*) FINAL
!~       WRITE(*,*) 'SURE?'
!~       READ(*,*) DECISION
!~       IF(DECISION=='N')THEN
!~        GOTO 28
!~       ENDIF
!~      ELSEIF(DECISION=='S') THEN
!~       CONTINUE
!~      ELSEIF(DECISION=='s') THEN
!~       CONTINUE
!~      ELSE
!~       WRITE(*,*) 'PLEASE TPYE CAPITAL LETTER'
!~       GOTO 27
!~      ENDIF

!計算長度
    OPEN(10,FILE=NAME(1))
    CHECK=0
    NO=0
    DO WHILE(CHECK==0)
     READ(10,*,IOSTAT=CHECK) DATE_T,RAIN_T
     IF((DATE_T>=START).AND.(DATE_T<=FINAL))THEN
      NO=NO+1
     ENDIF
    ENDDO
    NO=NO-1
    CLOSE(10)

    WRITE(*,*) '起始時間：',START
    WRITE(*,*) '結束時間：',FINAL
    WRITE(*,*) N,NO
    END

!--------------------------------------------------
    SUBROUTINE DAY5(NAME,N,NO,P,P5,START,FINAL)
    IMPLICIT NONE
    INTEGER N
    CHARACTER*30 NAME(N)
    INTEGER NO,I,START,FINAL,DATE_T,CHECK,J
    REAL P(N,NO),P5(N,NO),PB(5),RAIN_T
    DATA PB /0.,0.,0.,0.,0./

    DO I=1,N
     OPEN(10+I,FILE=NAME(I))
     CHECK=0
     J=0
     DO WHILE(CHECK==0)
      READ(10+I,*,IOSTAT=CHECK) DATE_T,RAIN_T
      IF((DATE_T>=START).AND.(DATE_T<=FINAL))THEN
      J=J+1
      IF( J>NO )  EXIT
      P(I,J)=RAIN_T
      P5(I,J)=PB(1)+PB(2)+PB(3)+PB(4)+PB(5)
      PB(5)=PB(4)
      PB(4)=PB(3)
      PB(3)=PB(2)
      PB(2)=PB(1)
      PB(1)=P(I,J)
      ENDIF
     ENDDO
     CLOSE(10+I)
    ENDDO

    END

!--------------------------------------------------
    SUBROUTINE INPUT_T(NAMET,N,NO,T,START,FINAL)
    IMPLICIT NONE
    INTEGER N
    CHARACTER*30 NAMET(N)
    INTEGER NO,START,FINAL,CHECK,J,DATE_T,I
    REAL T(N,NO),T_T

    DO I=1,N
     OPEN(10+I,FILE=NAMET(I))

     CHECK=0
     J=0

     DO WHILE(CHECK==0)
      READ(10+I,*,IOSTAT=CHECK) DATE_T,T_T
      IF(CHECK==0)THEN
      IF((DATE_T>=START).AND.(DATE_T<=FINAL))THEN
       J=J+1
       IF( J>NO )  EXIT
       T(I,J)=T_T
      ENDIF
      ENDIF
     ENDDO
     CLOSE(10+I)
    ENDDO

    END
!----------------------------------------------------
    SUBROUTINE CN(NAME,C,CN2,N,NO,START,FINAL,P5,M,Y,CN3)
    IMPLICIT NONE
    INTEGER N
    CHARACTER*30 NAME(N)
    INTEGER I,J,NO,START,FINAL,CHECK,DATE_T,Y_T,Y(NO),MD,M(NO)
    REAL C(N,NO),CN2(N),AM1,AM2,P5(N,NO),TEMP,CN1,CN3
    DO I=1,N
     CN1=4.2*CN2(N)/(10-0.058*CN2(N))
     CN3=23.*CN2(N)/(10+0.13*CN2(N))
     OPEN(10+I,FILE=NAME(I))
     CHECK=0
     J=0
     DO WHILE(CHECK==0)
      READ(10+I,'(I4,I4,F8.2)',IOSTAT=CHECK) Y_T,MD,TEMP
!~             write(*,'(I4,I4,F8.2)') Y_T,MD,TEMP
      DATE_T=Y_T*10000+MD
      IF(CHECK==0)THEN
      IF((DATE_T>=START).AND.(DATE_T<=FINAL))THEN
       J=J+1
       IF( J>NO )  EXIT
       Y(J)=Y_T
       M(J)=MD
       IF((MD>200).AND.(MD<1200))THEN   !生長期時
        AM1=3.56     !單位為公分
        AM2=5.21     !單位為公分
       ELSE                                   !休耕期時
        AM1=1.27     !單位為公分
        AM2=2.79     !單位為公分
       ENDIF

       IF(P5(I,J)<AM1)THEN
        C(I,J)=(CN2(N)-CN1)*P5(I,J)/AM1+CN1
       ELSEIF(P5(I,J)>AM2)THEN
        C(I,J)=CN3
       ELSE
        C(I,J)=(CN3-CN2(N))*(P5(I,J)-AM1)/(AM2-AM1)+CN2(N)
       ENDIF
      ENDIF
      ENDIF
     ENDDO
     CLOSE(10+I)
    ENDDO

    END
!------------------------------------------------------
    SUBROUTINE RUNOFF_ET_ESTIMATE_PC_UT(N,NO,C,P,Q,Y,T,M,ET,PAR,UT0,UR,UT,GT,ST0,R,PC,ST,CN3,O,SS)
    IMPLICIT NONE
    INTEGER N,NO,I,J,Y(NO),M(NO)
    REAL W,C(N,NO),P(N,NO),Q(N,NO)
    INTEGER K,SMALL,LARGE,PAR(N)
    REAL H(12,4),HT,T(N,NO),ET(N,NO),EOT,PE,CE(12,4),CC
    REAL IT,CN3
    REAL UT0,UR,UT(N,NO+1),GT(N,NO),ST0,R
    REAL PC(N,NO),ST(N,NO+1),O(N,NO),SS(N,NO)
    REAL K1,X,C0,C1,C2

    !WRITE(*,*)'K1,X'
    !READ(*,*)K1,X
    !C0=(-K1*X+0.5)/(K1*(1-X)+0.5)
    C0=1.
    !C1=(K1*X+0.5)/(K1*(1-X)+0.5)
    C1=0.
    !C2=1-C0-C1
    C2=0.
    WRITE(*,*)C0,C1,C2

    OPEN(5,FILE='H.CSV')
    OPEN(10,FILE='C.CSV')

    DO I=1,12
     READ(5,*) (H(I,J),J=1,4)
     READ(10,*) (CE(I,J),J=1,4)
    ENDDO
    CLOSE(5)
    DO I=1,N
     DO J=1,NO



      W=2540./C(I,J)-25.4
      IF(P(I,J)>0.2*W)THEN
       Q(I,J)=(P(I,J)-0.2*W)**2/(P(I,J)+0.8*W)
      ELSE
       Q(I,J)=0.
      ENDIF

      !地表馬式金耕法
      IF( J==1 )THEN
        O(I,J)=C0*Q(I,J)
        SS(I,J)=Q(I,J)-O(I,J)
      ELSE
        O(I,J)=C0*Q(I,J)+C1*Q(I,J-1)+C2*O(I,J-1)
        if( O(I,J)<0.0 )then
          O(I,J)=0.0
        end if
        SS(I,J)=SS(I,J-1)+Q(I,J)-O(I,J)
      END IF

      EOT=33.8639*((0.00738*T(I,J)+0.8072)**8-0.000019*(1.8*T(I,J)+48)+0.001316)
      SMALL=100
      LARGE=200
      K=1
      HT=H(K,PAR(I))
      CC=CE(K,PAR(I))
!決定月份
      DO WHILE(M(J)>LARGE)   !m(j)是日期
!~             DO WHILE((M(J)<SMALL).OR.(M(J)>LARGE))   !m(j)是日期
       SMALL=SMALL+100
       LARGE=LARGE+100
       K=K+1
       HT=H(K,PAR(I))
       CC=CE(K,PAR(I))
      ENDDO
      PE=(0.021*HT**2*EOT)/(T(I,J)+273)  !mm帶0.21 cm帶0.021
      IT=P(I,J)-Q(I,J)

      IF (J==1)THEN
        ET(I,J)=CC*PE
      ELSE IF ( UT(I,J)<=0.8*UR )THEN
        ET(I,J)=CC*UT(I,J)/(0.8*UR)*PE
!       ET(I,J)=CC*PE
      ELSE
        ET(I,J)=CC*PE
      END IF
      !加入降雨門檻
      IF( P(I,J)>1.0 )THEN
        ET(I,J)=0.0
      END IF

      IF(J==1)THEN
       UT(I,J)=UT0   !初始未飽和層含水量
       ST(I,J)=ST0   !初始飽和層含水量
      ENDIF

      IF(ET(I,J)>UT(I,J)+IT)THEN !蒸發量>未飽含水量+IT(IT只是一個參數)
       ET(I,J)=UT(I,J)+IT

       IF(UT(I,J)+IT-ET(I,J)-UR>0.)THEN  !UR根層琛度最大含水量
        PC(I,J)=UT(I,J)+IT-ET(I,J)-UR         !PC滲漏量
       ELSE
        PC(I,J)=0.
       ENDIF
      ELSE
       IF(UT(I,J)+IT-ET(I,J)-UR>0.)THEN
        PC(I,J)=UT(I,J)+IT-ET(I,J)-UR
       ELSE
        PC(I,J)=0.
       ENDIF
      ENDIF

      UT(I,J+1)=UT(I,J)+IT-ET(I,J)-PC(I,J)   !I是測站 J為日期   算未飽和層變動
      GT(I,J)=R*ST(I,J)                      !R是退水係數 ST是飽和層含水量
      ST(I,J+1)=ST(I,J)+PC(I,J)-GT(I,J)                !算飽和層變動
      IF(ST(I,J+1)<0.)THEN
       ST(I,J+1)=0.
      ENDIF
     ENDDO
     GT(I,NO)=R*ST(I,NO)                      !最後的流量
    ENDDO





    END

!------------------------------------------------------
    SUBROUTINE SF_OUTPUT(N,NO,Q,GT,WEIGHT,START,FINAL,AREA,Y,M,O)
    IMPLICIT NONE
    CHARACTER*10 DAY(31)
    INTEGER I,J,K,N,NO,Y(NO),M(NO),SMALL,LARGE,START,FINAL,NN,DATE_C
    REAL SF(NO),Q(N,NO),GT(N,NO),WEIGHT(N),SFD(31),SFT,SFM,AREA,O(N,NO)
    INTEGER NUM
    REAL SFO(NO),DIS(NO),SA,OA,SAV,OAV,SUM1,SUM2,SUM3,SUM4,R2,NASH,RMSE,Z(NO)
    REAL,ALLOCATABLE::A(:,:)

    OPEN(10,FILE='RESULT.TXT')
    OPEN(11,FILE='RESULT-D.TXT')
!~     OPEN(12,FILE='QOB.TXT')
!~     OPEN(77,FILE='MKTEST.TXT')
!~     OPEN(78,FILE='MKO.TXT')
    OPEN(5,FILE='TEMP.TXT')

!~     DO J=1,NO
!~       READ(12,*)SFO(J)
!~     END DO

    DO I=1,31
     WRITE(5,'(I2,A2)') I,'日'
    ENDDO
    REWIND(5)

    DO I=1,31
     READ(5,*) DAY(I)
    ENDDO
    CLOSE(5)

    WRITE(10,'(A8,32A10)') ADJUSTR('年月'),(ADJUSTR(DAY(I)),I=1,31),ADJUSTR('月合計')
!以上只有一行 就是輸出檔最上面那一個

    DO J=1,NO
     SF(J)=0
     DO I=1,N
!     SF(J)=SF(J)+(O(I,J)+GT(I,J))*WEIGHT(I)*AREA*10000/86400
      SF(J)=SF(J)+(O(I,J)+GT(I,J))*WEIGHT(I)
!將地表流量+地下流量後乘上權重*面積/時間  得到單位為CMS
      Z(J)=Z(J)+GT(I,J)*WEIGHT(I)


!
     ENDDO
     Z(J)=SFO(J)-Z(J)
     IF( Z(J)<0 )THEN
        Z(J)=0
     END IF
        WRITE(78,*)Z(J)
    ENDDO
!~
!~     !計算MK法檢定如下
!~     !範圍為1990~1998資料
!~     NUM=0
!~     DO J=366,5481
!~         WRITE(77,*) SF(J),SFO(J)
!~         NUM=NUM+1
!~     END DO
!~     write(*,*)NUM
!~     CLOSE(77)
!~     ALLOCATE(A(2,NUM))
!~
!~
!~     OPEN(77,FILE='MKTEST.TXT')
!~     !將觀測值與模擬值寫到矩陣中
!~     DO  J=1,NUM
!~       READ(77,*) (A(I,J),I=1,2)
!~     END DO
!~
!~     SA=0
!~     OA=0
!~     !計算平均值
!~     DO  J=1,NUM
!~       SA=SA+A(1,J)
!~       OA=OA+A(2,J)
!~     END DO
!~     SAV=SA/NUM
!~     OAV=OA/NUM
!~
!~     !計算R2
!~     !計算NASH
!~     !計算RMSE
!~     DO  J=1,NUM
!~       SUM1=SUM1+( (A(1,J)-SAV)*(A(2,J)-OAV) )
!~       SUM2=SUM2+( (A(2,J)-OAV)**2 )
!~       SUM3=SUM3+( (A(1,J)-SAV)**2 )
!~       SUM4=SUM4+( (A(1,J)-A(2,J))**2 )
!~     END DO
!~
!~     R2=( SUM1/((SUM2*SUM3)**0.5) )**2
!~     NASH=1-SUM4/SUM2
!~     RMSE=(SUM4/NUM)**0.5
!~     !WRITE(*,*)'R2',R2
!~     !WRITE(*,*)'NASH',NASH
!~     !WRITE(*,*)'RMSE',RMSE



    DO I=Y(1),Y(NO)           !Y為年份
     DO J=1,12
      SMALL=I*10000+J*100
      LARGE=I*10000+(J+1)*100
      IF((SMALL>START-100).AND.(SMALL<FINAL))THEN
       NN=0
       DO K=1,31
        SFD(K)=-9999                                 !此舉目地在於先將此月每天以-9999算 之後再取代
       ENDDO

       SFT=0.
       DO K=1,NO
        DATE_C=Y(K)*10000+M(K)                       !打出年月日
        IF((DATE_C>SMALL).AND.(DATE_C<LARGE))THEN
         NN=NN+1                                     !計算當月天數
         SFD(NN)=SF(K)                               !SF為日流量
         SFT=SFT+SFD(NN)                             !計算當月流量
        ENDIF
       ENDDO

       SFM=SFT/REAL(NN)                              !計算月平均流量
!      WRITE(10,'(I8,32I10)') SMALL/100,(INT(SFD(K)),K=1,31),INT(SFT)     !年月,日流量,總流量
       WRITE(10,'(I8,32f10.4)') SMALL/100,((SFD(K)),K=1,31),SFT

       DO K=1,31
        IF(SFD(K).NE.-9999)THEN
!        WRITE(11,'(I10,I8)') SMALL+K,INT(SFD(K))
         WRITE(11,'(I10,f10.4)') SMALL+K,SFD(K)
        ENDIF
       ENDDO

      ENDIF
     ENDDO
    ENDDO

    CLOSE(10)
!~     close(77,STATUS='DELETE')
!~     close(78,STATUS='DELETE')

    END
