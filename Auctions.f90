       PROGRAM PRINCIPAL 
       IMPLICIT NONE          
       INTEGER i,j,r,n(10),k,iter,iterv(10),sem,a,b,d,e,f,numcortes,iter2,kmax2,l,h,m,qa,qa1
       INTEGER g, gsalida,salida
       INTEGER g2, gsalida2,salida2,g3,gsalida3,salida3,salidaSM,gsalidaSM,gSM
       REAL*16 p(10),a1(10),b1(10),p1(10)
       REAL*8 oferta(1000),ranf,c,num,PrecSegundo
       real*8 cmax2,vcorte,maxiOptima,PrecOptima,vuelta
       real*8 vueltaOptima, maxvuelta, maxiSegundo,voptima,num2,mediana 
       real*8 maxiC,fmax2,PrecC 
       real*8 maxiM,PrecM,X0,X1,XMIN,X3
       real*16 PrecOptima2, PrecSegundo2,PrecC2,PrecM2, VarOptima, VarSegundo, VarNuestra, VarM
       real*16 DesOptima, DesSegundo, DesNuestra, DesM,DesMSM, ContNuestra, ContOptima, ContM
       real*16 A2,B2,Apr,Bpr,Cpr,y11,y22,x11,x22,Y0
       real*16 aIni, aFin
       integer i1,j1

    
!cuando vayamos a leer los archivos en un sas nos generamos un fichero para cada variable y luego los juntamos, incluyen p,a,b,PRECIO(ó beneficio)
!no hae falta imprimir la n porque en principio será para n=20	
       OPEN (1,file='Precio_Opt_p5_a4_b4_n4.txt') 
       Open (22,file='Precio_Segundo_p5_a4_b4_n4.txt')
       OPEN (3,file='Precio_Nuestra_p5_a4_b4_n4.txt') 
       Open (4,file='Precio_MY_p5_a4_b4_n4.txt')

open(30,file='nuestra_p5_a4_b4_n4')
open(40, file='Myerson_p5_a4_b4_n4')
open(20,file='test_p5_a4_b4_n4')
open(100,file='parametros_p5_a4_b4_n4')
       
!$$$$$$ oferta(1)=0.4
!$$$$$$ oferta(2)=0.35
!$$$$$$ oferta(3)=0.3   
!$$$$$$ oferta(4)=0.22
!$$$$$$ oferta(5)=0.21
!$$$$$$ oferta(6)=0.2
!$$$$$$ oferta(7)=0.2
!$$$$$$ oferta(8)=0.19
!$$$$$$ oferta(9)=0.18
!$$$$$$ oferta(10)=0.017
!$$$$$$ oferta(11)=0.016
!$$$$$$ oferta(12)=0.015    
!$$$$$$ oferta(13)=0.014
!$$$$$$ oferta(14)=0.013
!$$$$$$ oferta(15)=0.013
!$$$$$$ oferta(16)=0.012
!$$$$$$ oferta(17)=0.012
!$$$$$$ oferta(18)=0.012
!$$$$$$ oferta(19)=0.012
!$$$$$$ oferta(20)=0.01
!$$$$$$           
!19     format(255(1x,f10.8)) 
        sem=13
       
       ! k=2
       ! n=10
       ! c=19.8      
       ! iter=1000000 
     !   epsilon=0.000001
     
     !iterv := iteracion
        iterv(1)=1000
        iterv(2)=10000
        iterv(3)=100000
        iterv(4)=1000000
        iterv(5)=10000000
        iterv(6)=100000000
        iterv(7)=1000000000
        
     !p := probabilidad de que pujas altas, p2 en el ejemplo
        p(1)=0.01
        p(2)=0.02
        p(3)=0.05
        p(4)=0.1
        p(5)=0.2

     !p1 := probabilidad de pujas bajas, p1 en el ejemplo
        p1(1)=0.99
        p1(2)=0.98
        p1(3)=0.95
        p1(4)=0.9
        p1(5)=0.8
        
  	 !n := numero de jugadores
        n(1)=10
        n(2)=20
        n(3)=50
        n(4)=100

     !a1 := limite inferior del intervalo   
        a1(1)=0.1
        a1(2)=0.2
        a1(3)=0.3
        a1(4)=0.4
        a1(5)=0.5

	 !b1 := limite superior del intervalo
        b1(1)=0.9
        b1(2)=0.8
        b1(3)=0.7
        b1(4)=0.6
        b1(5)=0.5
        
 		!rejilla
   	    numcortes=50
		voptima=0
        vueltaOptima=00
        DO  r=2,2 !para recorrer el numero de iteraciones
          do a=4,4 !n   para recorrer los posibles valores de n
           do b=5,5  !p para recorrer los posibles valores de p (o p1, sirve el mismo contador pues son complementarios)
            do e=4,4 ! valor de extremo inferior del intervalo(para recorrer los posibles valores del vector b1)
             do d=4,4! valor del extremo superior del intervalo (para recorrer los posibles valores del vector a1)
               
              maxiOptima=0
              maxiSegundo=0  
              maxiC=0
              maxiM=0
              maxvuelta=(n(a)-3)*numcortes+numcortes+1 
              do k=2,(n(a)-1)
               do f=1,numcortes+1 ! porcentaje para c 
                  sem=13
                  write(*,*) 'n=',a,'p=',b,'a1=',d,'b1=',e, 'k',k,'f',f
                  vuelta=(k-2)*numcortes+f
                  if (vuelta<maxvuelta*(1./2)) then
                    vcorte=((vuelta-1)*a1(d)/(maxvuelta*(1./2)-1.5))
                  else
                    vcorte=(b1(e))+((vuelta-(maxvuelta*(1./2)+0.5))*(1-b1(e))/(maxvuelta*(1./2)-0.5))
                  endif  
                   c=((f-1)*(1./numcortes)*(1-(1./k)))
              
                  iter=iterv(r)            
                  
                  PrecSegundo=0
                  PrecOptima=0
                  PrecC=0
                  PrecM=0
                 
                   do i=1,iter
                    do j=1,n(a)
                      num=ranf(sem)
                      num2=ranf(sem)
                      if (num<p(b)) then
                         oferta(j)=(b1(e))+((1-b1(e))*num2)
                      else
                         oferta(j)=(a1(d)*num2)
                      endif    
                    enddo
                      
                    call ORDEN(n(a),oferta) 
                    ! subasta optima
                    if (oferta(1)<vcorte) then
                       PrecOptima=PrecOptima+(0/iter)
                    elseif (oferta(2)<vcorte) then   
                       PrecOptima=PrecOptima+(vcorte/iter)
                    else   
                       PrecOptima=PrecOptima+(oferta(2)/iter)
                    endif  
                    !subasta a segundo precio                                    
                      PrecSegundo=PrecSegundo+(oferta(2)/iter)
                    ! subasta nuestra
                    
                    if (((k-1)*oferta(1))+oferta(k+1)<=(k*c)) then
                       PrecC=PrecC+(oferta(k+1)/iter)
                    elseif (((k-1)*oferta(2))+oferta(k+1)<=(k*c)) then
                       PrecC=PrecC+(c/iter)
                    else
                       PrecC=PrecC+(oferta(2)/iter)
                    endif
                    
                 !definir los máximos                  
                 if (PrecC>maxiC) then
                   maxiC=PrecC
                   kmax2=k
                   cmax2=c
                   fmax2=f
                 endif
              
                 !write(*,*)"precoptima",precoptima, "maxioptima",maxioptima,"voptima",voptima
                 !pause
                 if (PrecOptima>maxiOptima) then
                   maxiOptima=PrecOptima
                   Vueltaoptima=vuelta
                   voptima=vcorte
                 endif 
                 
                 if (PrecSegundo>maxiSegundo) then
                   maxiSegundo=PrecSegundo
                 endif
                 
                 if (PrecM>maxiM) then
                   maxiM=PrecM
                 endif
              enddo
              enddo  
              enddo
              
        !!!!!!!!!!!!!!!! una vez tenemos los parametros optimos, recalculamos todas las subastas con ellos, y obtenemos la varianza y la desviacion tipica
               PrecOptima2=0
               PrecSegundo2=0
               PrecC2=0
               PrecM2=0
               
			!Parametro para controlar la eficiencia (CANTIDAD DE VECES QUE SE LO LLEVA EL MEJOR PUJADOR)
               ContNuestra=0
               ContOptima=0
               ContM=0 !suma 1 si el pujador más alto esta en zona de sorte, 1/#jugadores en zona de sorteo, caso 5
               
			!Varianza de las distintas subastas
               VarOptima=0
               VarSegundo=0
               VarNuestra=0
               VarM=0
               DesM=0

               iter2=10000	
write(30,*)'==============================================================='
              write(30,*) '==Simulación con==',iter2, '   iteraciones'                  
              write(30,*) 'Jugadores',n(a),'Prob',p(b),'a',a1(d),'b',b1(e)
              write(30,*) "k", kmax2, "c",cmax2
               
              write(40,*) '==Simulación con==',iter2, '   iteraciones'                  
              write(40,*) 'Jugadores',n(a),'Prob',p(b),'a',a1(d),'b',b1(e)
              
               sem=13
             ! bucle que me recorra los valores que deseemos de a1 y a2
!$$$$$$           do i1=1,5
!$$$$$$             do j1=1,i1
!$$$$$$              aFin=(a1(d)*j1)/i1
!$$$$$$              aIni=(a1(d)/i1)*(j1-1)
!$$$$$$              write(30,*) 'aIni', aIni, 'aFin',aFin
!$$$$$$              write(40,*) 'aIni', aIni, 'aFin',aFin
             !write (*,*) 'i1',i1,'j1',j1,'aini',aini,'afin',afin
             !pause
             do i=1,iter2
                  do j=1,n(a)
                    num=ranf(sem) !primer numero aletaorio
                    num2=ranf(sem)
                    if (num<p(b)) then !estas en la primera montaña, puja baja
                       oferta(j)=(b1(e))+((1-b1(e))*num2) !estas en la segunda montaña,puja alta
                    else
                       oferta(j)=(a1(d)*num2) !oferta(j)=aini+((afin-aini)*num) !
                    endif    
                    
                  enddo
                  
                    call ORDEN(n(a),oferta) 
                    PrecSegundo2=PrecSegundo2+(oferta(2)/iter2)
                    
                    VarSegundo=VarSegundo+((oferta(2)*oferta(2))/iter2)

                    write(30,*) '============================================'
                do qa=1,kmax2+1
                  write(30,*) "oferta(",qa,")=", oferta(qa)
                  enddo
                  write(30,*) "Inecuacion",(oferta(2)-cmax2),">",((oferta(2)-oferta(kmax2+1))*(1./kmax2))
                  
                    if (((kmax2-1)*oferta(1))+oferta(kmax2+1)<=(kmax2*cmax2)) then !sorteo entre las k mayores pujas,caso c)
                        write(30,*) "subasta nuestra, caso c) sorteo, precio=oferta(k+1)",oferta(kmax2+1)
						PrecC2=PrecC2+(oferta(kmax2+1)/iter2)
                        contNuestra=contNuestra+(1./kmax2)
                        VarNuestra=VarNuestra+((oferta(kmax2+1)*oferta(kmax2+1))/iter2)
                        
                    elseif (((kmax2-1)*oferta(2))+oferta(kmax2+1)<=(kmax2*cmax2)) then !caso b)
                        write(30,*) "subasta nuestra, caso b), cmax2",cmax2
                       PrecC2=PrecC2+(cmax2/iter2)
                       contNuestra=contNuestra+1
                       VarNuestra=VarNuestra+((cmax2*cmax2)/iter2)

                    else !caso a)
                        write(30,*) "subasta nuestra, caso a), oferta(2)",oferta(2)                  
                       PrecC2=PrecC2+(oferta(2)/iter2)
                       contNuestra=contNuestra+1
                       VarNuestra=VarNuestra+((oferta(2)*oferta(2))/iter2) 
                    endif
                      
                    mediana=(0.5*a1(d))/(1-p(b))   !matematicamente ya conocemos el punto de corte optimo
                    !conocemos matematicamente la voptima, va a ser o el punto de corte donde considero que empiezan las ventas altas,b
                    !o en el centro de la primera montaña
                    !para evitar aproximaciones, nos quedamos, segun el caso, con el mas cercano
                    if (abs(mediana-voptima)<abs(b1(e)-voptima)) then
                      voptima=mediana
                    else
                      voptima=b1(e)
                    endif
                        
                    if (oferta(1)<voptima) then
                       PrecOptima2=PrecOptima2+(0/iter2) !no lo ha venido
                    elseif (oferta(2)<voptima) then   
                       PrecOptima2=PrecOptima2+(voptima/iter2)
                       contOptima=contOptima+1  
                       VarOptima=VarOptima+((voptima*voptima)/iter2)
                    else   
                       PrecOptima2=PrecOptima2+(oferta(2)/iter2)
                       contOptima=contOptima+1
                       VarOptima=VarOptima+((oferta(2)*oferta(2))/iter2)
                    endif                                     
                  
                  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MYERSON
                  write(40,*) '======================================'

                    do qa1=1,kmax2+1
                  write(40,*) "oferta(",qa1,")=", oferta(qa1)
                  enddo
                  !calculamos x1,x2,y1,y2 (para ello necesitamos A,B, A'(A1), B'(B1), C'(C1)
                  A2=((1-b1(e))*p1(b))/(a1(d)*p(b))
                  B2=-(((b1(e)-((1-b1(e))*(1+p1(b)))/(p(b)))*(p1(b)/(2*a1(d))))+0.5)
                  
                  Apr=((-a1(d)/p1(b))*(A2*A2))+((1-b1(e))/p(b))
                  Bpr=(a1(d)*2*A2*B2)/p1(b)
                  Cpr=((-a1(d)*B2*B2)/p1(b))+b1(e)-(((1-b1(e))*p1(b))/p(b))
                  
                  y11=(-Bpr+SQRT(Bpr*Bpr-4*Apr*Cpr))/(2*Apr)
			   	  y22=(-Bpr-SQRT(Bpr*Bpr-4*Apr*Cpr))/(2*Apr)
					
                  x11=A2*y11-B2
                  x22=A2*y22-B2

                  if (((x11.ge.0).and.(x11.lt.p1(b))).and.((y11.gt.p1(b)).and.(y11.le.1))) then
                    X0=x11
                    Y0=(((1-b1(e))*(y11-p1(b)))/p(b))+b1(e)
                  elseif (((x22.ge.0).and.(x22.lt.p1(b))).and.((y22.gt.p1(b)).and.(y22.le.1))) then
                    X0=x22
                    Y0=(((1-b1(e))*(y22-p1(b)))/p(b))+b1(e)                     
				  else
                  	X0=p1(b)-sqrt((p1(b)*p1(b))-p1(b)-((p1(b)*b1(e)*(p1(b)-1))/a1(d)))
                    Y0=b1(e)
                  endif
                  
                     XMIn=a1(d)/(2*p1(b)) !es el valor minimo de venta
                     X3=(a1(d)*X0)/p1(b)  !limite inferior del intervalo que define la zona de sorteo
write(40,*) 'x0=',x0,' x3=',x3,' xmin=', xmin
                     if (X0.le.0) then !CASO 1
                       if (oferta(1).ge.b1(e)) then
                         if (oferta(2).ge.b1(e)) then
                           PrecM2=PrecM2+(oferta(2)/iter2)
                           VarM=VarM+((oferta(2)*oferta(2))/iter2)
                           ContM=ContM+1       
                         else
                           PrecM2=PrecM2+(b1(e)/iter2)
                           VarM=VarM+((b1(e)*b1(e))/iter2)
                           ContM=ContM+1
                         endif
                       endif
                     
					 else ! CASO 2 X0>0	
                       X1=a1(d)*((2*X0)-1)/p1(b) !segundo trozo de la funcion cbarra en el CASO 2, cbarra(t)=a*(2X0-1)/p1

                       if (X1<0) then
                         if (oferta(1).ge.Y0) then
                         	if (oferta(2).ge.Y0) then
                           		PrecM2=PrecM2+(oferta(2)/iter2)
                                VarM=VarM+((oferta(2)*oferta(2))/iter2)
                                ContM=ContM+1  
                         	else
                           		PrecM2=PrecM2+(Y0/iter2)
                                VarM=VarM+((Y0*Y0)/iter2)
                                ContM=ContM+1
                         	endif
                         endif !if oferta(1)< b no se vende
                       else !X1>=0
                         !XMIn=a1(d)/(2*p1(b)) !es el valor minimo de venta
                         !X3=(a1(d)*X0)/p1(b)  !limite inferior del intervalo que define la zona de sorteo
                         
 						! CASO 1) en el ejemplo	
 						 if (((oferta(1).gt.Y0).and.(oferta(2).gt.Y0)).or.&
                         ((oferta(2).gt.Xmin).and.(oferta(2).lt.X3))) then
                           !.and.(oferta(1)>XMin)).and..or.(oferta(2)>XMin)) then 
                           !OJO CREO QUE AQUI HAY QUE AÑADIR QUE NINGUNO ESTAN EN EL INTERVALO DE NO COMPRA
                         !ni oferta(1) ni oferta(2) estan en la zona de sorteo, [X3,b], ni en zona de NO compra, [0,XMin)

                           PrecM2=PrecM2+(oferta(2)/iter2) !se lo lleva el mejor pujador al segundo precio (quien haya pujado (oferta(1), pagando oferta(2))

						   VarM=VarM+((oferta(2)*oferta(2))/iter2)
                           ContM=ContM+1
write(40,*) "caso1, precio= oferta(2)",oferta(2)
 						! CASO 2) en el ejemplo                           
                         elseif ((oferta(2)<XMin).and.(oferta(1).ge.XMin)) then
                         !la segunda puja mas alta esta en el intervalo de NO compra,[0,XMin), y la puja mas alta si esta en el intervalo de compra
                         
                           PrecM2=PrecM2+(XMin/iter2) !se lo lleva el mejor pujador pagando XMin

						   VarM=VarM+((XMin*XMin)/iter2)
                           ContM=ContM+1
write(40,*) "caso2, precio=xmin",xmin
!pause
 						! CASO 4) en el ejemplo
                         elseif ((oferta(1).gt.Y0).and.((oferta(2).le.Y0).and.(oferta(2).ge.X3))) then
                         !write(*,*)"caso 4"
                         !pause
                            !la oferta(1)>b y oferta(2) esta en el intervalo [X3,b] (zona de sorteo)
                            g=2
                            gsalida=2 !contador para calcular el numero de pujadores que han hecho una oferta en el intervalo [X3,b]
                            salida=0
                            !write(*,*) "n(a)",n(a),"oferta(gsalida)",oferta(10),"b1(e)",b1(e),"x3",x3
                            !pause
                            do while ((salida==0).and.((oferta(gsalida).le.Y0).and.(oferta(gsalida).ge.X3))) !mientras la oferta que estamos analizando este dentro de la zona de sorteo, o no forcemos la salida
                              g=g+1
                              gsalida=g
                              !write(*,*) "oferta(gsalida)",oferta(gsalida),"gsalida",gsalida,"g",g
                              !pause
                              if (g==(n(a)+1)) then
                                salida=1 !para salir del contador 
                                !write(*,*) "salida=1, me salgo"
                                !pause
                                !gsalida=n(a)
                                !g=n(a)+1
                              endif 
                            enddo  !en el sorteo habrá gsalida-2 jugadores (el primer pujador no esta en el sorteo)
                             !write (*,*) "gsalida",gsalida
                             !pause
                            PrecM2=PrecM2+ ((Y0-((Y0-X3)/(gsalida-1)))/iter2)
 
                            VarM=VarM+(((Y0-((Y0-X3)/(gsalida-1)))*(Y0-((Y0-X3)/(gsalida-1))))/iter2)
                            ContM=ContM+1
                            write(40,*) "caso4, precio=",(Y0-((Y0-X3)/(gsalida-1)))

                        ! CASO 5) en el ejemplo    
                         elseif (((oferta(1).le.Y0).and.(oferta(1).ge.X3)).and.((oferta(2).le.Y0).and.(oferta(2).ge.X3))) then
						!las dos primeras ofertas estan en el intervalo [X3,b]
                            PrecM2=PrecM2+(X3/iter2)!se sortea entre todos los pujadores cuya oferta esta en el intervalo [X3,b], y el precio a pagar X3
                            VarM=VarM+((X3*X3)/iter2)
                            write(40,*)"caso5 sorteo, precio=x3",x3
							!tenemos que conocer el numero de jugadores en el sorteo
                            g2=3
                            gsalida2=3 !contador para calcular el numero de pujadores que han hecho una oferta en el intervalo [X3,b]
                            salida2=0
                            do while ((salida2==0).and.((oferta(gsalida2).le.Y0).and.(oferta(gsalida2).ge.X3))) !mientras la oferta que estamos analizando este dentro de la zona de sorteo, o no forcemos la salida
                              g2=g2+1
                              gsalida2=g2
                              if (g2==(n(a)+1)) then
   								salida2=1 !para salir del contador
                                !gsalida2=n(a)
                                !g2=n(a)+1
                              endif
                            enddo !en este caso habra gsalida-1 jugadores en el sorteo (el primer pujador si esta en el sorteo)

                            ContM=ContM+(1./(gsalida2-1))
                         endif     
                       endif     
                  endif
                
                  enddo
                  contOptima=(ContOptima/iter2)
                  ContNuestra=(ContNuestra/iter2)
                  ContM=(ContM/iter2)
                  
                  VarOptima=VarOptima-(PrecOptima2*PrecOptima2)
                  VarSegundo=VarSegundo-(PrecSegundo2*PrecSegundo2)
                  VarNuestra=VarNuestra-(PrecC2*PrecC2)
                  
                  !DesOptima=sqrt(VarOptima)
                  !DesSegundo=sqrt(VarSegundo)
                  !DesNuestra=sqrt(VarNuestra)
                  !DesM=sqrt(VarM)
                  
        !!!!!!!!!!!!!!!!! 

!$$$$$$ write(1,*) a1(d),aIni,aFin,PrecOptima2!,'OPTIMA'
!$$$$$$ write(22,*) a1(d),aIni,aFin,PrecSegundo2!'SEGUNDO'
!$$$$$$ write(3,*) a1(d),aIni,aFin,PrecC2!'NUESTRA'
!$$$$$$ write(4,*) a1(d),aIni,aFin,PrecM2!'MYERSON'
        
write(1,*) p(b),a1(d),b1(e),PrecOptima2!,BenOptima2!,'OPTIMA'
write(22,*) p(b),a1(d),b1(e),PrecSegundo2!,BenSegundo2,'SEGUNDO'
write(3,*) p(b),a1(d),b1(e),PrecC2!,BenC2,'NUESTRA'
write(4,*) p(b),a1(d),b1(e),PrecM2!,BenM2,'MYERSON'
write(100,*) 'A=',A2,'B',B2,'Aprima',Apr,'Bprima',Bpr,'Cprima',Cpr,'x1',x11,'y1',y11,'x2',x22,'y2',y22    



              !write(20,*) '==Simulación con==',iterv(r), '   iteraciones' 
              write(20,*) '==Simulación con==',iter2, '   iteraciones'                  
              write(20,*) 'Jugadores',n(a),'Prob',p(b),'a',a1(d),'b',b1(e)
                ! write(40,*) 'aIni', aIni, 'aFin',aFin 
!$$$$$$               write(20,*) 'maximo porcentaje de c*',fmax2,'c* maximo',cmax2,'maximo k*',kmax2
!$$$$$$               write(20,*) 'maximo porcentaje de v*',vueltaOptima,'valor de v*', voptima
!$$$$$$               
!$$$$$$               write(20,*) '% eficiencia de la de precio mínimo:', ContOptima
!$$$$$$               Write(20,*) '% eficiencia de la nuestra con c*:', ContNuestra
!$$$$$$               Write(20,*) '% eficiencia de la de Myerson: ', ContM
!$$$$$$               Write(20,*) '% eficiencia de la de Myerson sin mínimo: ', ContMSM
!$$$$$$               write(20,*) '======================================'
!$$$$$$               write(20,*) 'Varianza de la de precio mínimo:', VarOptima
!$$$$$$               write(20,*) 'Varianza de la de segundo precio es:', VarSegundo
!$$$$$$               Write(20,*) 'Varianza de la nuestra con c*:', VarNuestra
!$$$$$$               Write(20,*) 'Varianza de Myerson: ', VarM
!$$$$$$               Write(20,*) 'Varianza de Myerson sin mínimo: ', VarMSM
!$$$$$$               write(20,*) '======================================'
              
!$$$$$$               write(20,*) 'Varianza y d.t. de la de precio mínimo:', VarOptima, DesOptima
!$$$$$$               write(20,*) 'Varianza y d.t. de la de segundo precio es:', VarSegundo, DesSegundo
!$$$$$$               Write(20,*) 'Varianza y d.t. de la nuestra con c*:', VarNuestra, DesNuestra
!$$$$$$               Write(20,*) 'Varianza y d.t. de Myerson: ', VarM, DesM
!$$$$$$               write(20,*) '======================================'
              
              write(20,*) 'El precio de la de precio mínimo:', PrecOptima2
              write(20,*) 'El precio medio de la de segundo precio es:', PrecSegundo2
              Write(20,*) 'El precio medio de la nuestra con c*:', PrecC2
              Write(20,*) 'El precio medio de Myerson: ', PrecM2
              write(20,*) '======================================'        
          !enddo
         enddo
        enddo  
       enddo
       enddo                    
      ! enddo
      ! enddo
      stop 
      enddo
      ! enddo             
end







       
                   
! ======================================================
! Este programa genera numeroa aleatorios
! ======================================================
!       Variables de entrada

   
                                                                                      
! =======================================================   
! Función ranf 
!
! PROPÓSITO: Genera números aleatorios uniformes en (0,1)
! Adaptación de: ran()(NUMERICAL RECIPES)
!
! ARGUMENTOS DE ENTRADA/SALIDA:
! INTEGER::sem <=>semilla(>0)
!
! VALOR DE LA FUNCIÓN 
! REAL*8::ranf<=>número aleatorio en (0,1)
! =======================================================   
                 
      REAL*8 FUNCTION ranf(sem)         
      
      IMPLICIT NONE
      
!     Variables de entrada

      INTEGER sem      
      
!     Variables auxiliares
                 
      INTEGER ia, im, iq, ir, k
      REAL*8 am 
      
      ia = 16807
      im = 2147483647
      am = 1.d0/im
      iq = 127773
      ir = 2836
             
      k = sem/iq 
     
      sem = ia*(sem-k*iq)-ir*k
     
      IF (sem.lt.0) sem = sem+im
      ranf = am*sem
     
      RETURN 
        
      END FUNCTION ranf 

     SUBROUTINE ORDEN(NELEM,ARREG)
        ! -----------------------------------------------------
        ! ORDENACION POR BURBUJA ("buble sort") MEJORADO
        ! de un arreglo unidimensional, de mayor a menor.
        !
        ! NELEM = Número de elementos del arreglo
        ! ARREG = Arreglo unidimensional a ordenar
        ! -----------------------------------------------------
        IMPLICIT NONE
        INTEGER NELEM
        REAL*8 ARREG(*)
        ! -----------------------------------------------------
        LOGICAL CLAVE
        INTEGER l,m
        REAL AUX
        ! -----------------------------------------------------
        IF (NELEM.LT.2) RETURN
        l = 1
        CLAVE = .TRUE.
        DO WHILE(CLAVE)
          CLAVE = .FALSE.
          DO m=1,NELEM-l
           IF (ARREG(m).LT.ARREG(m+1)) THEN
           AUX = ARREG(m)
           ARREG(m) = ARREG(m+1)
           ARREG(m+1) = AUX
           CLAVE = .TRUE.
         ENDIF
         ENDDO
         l = l+1
        END DO
        RETURN
      END
              
                             
     
     
     
       
