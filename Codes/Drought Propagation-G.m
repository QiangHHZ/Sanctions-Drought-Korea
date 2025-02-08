clc;clear;
% ================================Load the data======================================================
load ('RESPI.mat');    
load ('RENDVI.mat');  % CSIF EVI NIRv LWSI

% ================================Calculate the propagation time======================================================
for i = 1:24
    for ii = 1:Size(SPI,2)  
        SPI_S = RESPI(1+24*(i-1):24+24*(i-1),ii);  
        NDVI_S = RENDVI(:,ii);
        [r,p] = corr(SPI_S(:,1),NDVI_S(:,1),'Type','Spearman'); 
        Rmatrix(i,ii) = [r];
        Pmatrix(i,ii) = [p];
        if i == 24
            if isnan(p)
                PT(1,ii) = NaN;
                bestSPI(:,ii) = RESPI(1:24,ii);
                bestP(:,ii) = NaN;
            else
                maxR(1,ii) = max( Rmatrix( : , ii ) ); 
                RR = find( Rmatrix(:,ii) == maxR(1,ii));
                if length(RR) > 1
                    PT(1,ii) = find( Rmatrix(:,ii) == maxR(1,ii),1) +  length(RR) - 1;  
                    bestP(:,ii) = Pmatrix(RR(end),ii);
                    bestSPI(:,ii) = RESPI(1+24*(RR(end) - 1 ):24+24*(RR(end)-1),ii);  
                else
                    PT(1,ii) = find( Rmatrix(:,ii) == maxR(1,ii)) ;  
                    bestP(:,ii) = Pmatrix(find(Rmatrix( : , ii ) == max( Rmatrix( : , ii ) )),ii);
                    bestSPI(:,ii) = RESPI(1+24*(find( Rmatrix(:,ii) == maxR(1,ii) ) - 1 ):24+24*(find( Rmatrix(:,ii) == maxR(1,ii))-1),ii);  
                end
            end
        end
    end
end

% ==========================Calculate the empirical distribution function============================================================
SizeNDVI = size(RENDVI);
D_NDVI = SizeNDVI(1); T_NDVI = SizeNDVI(2);
V1 = [];   
for i = 1:T_NDVI   
    Y = RENDVI(:,i);
    [fy,ysort] = ecdf(Y);  
    V1(:,i) = spline(ysort(2:end),fy(2:end),Y);  
end
SizeSPI = size(bestSPI);
Y_SPI = SizeSPI(1); EGS_SPI = SizeSPI(2);
U1 = zeros(Y_SPI,EGS_SPI); 
for i = 1:EGS_SPI
    X = bestSPI(:,i);
    [fx,xsort]=ecdf(X);
    U1(:,i) = spline(xsort(2:end),fx(2:end),X);
end
% ============================Calculate the empirical copula function value matrix==========================================================
CUV = zeros(size(U1));
for i = 1:EGS_SPI
    xx = U1(:,i);yy=V1(:,i);
    C = @(u,v)mean((xx<=u).*(yy<=v));
    for ii = 1:numel(xx)
        CUV(ii,i) = C(xx(ii),yy(ii));
    end
end
% ==============================Calculate the matrix of marginal distribution function values========================================================
V2 = [];
p = [];h = [];rmse = [];AllRHP = [];AllPosi = [];bestndvi = [];Nodistribution = [];
for i = 1:EGS_SPI 
    Y = RENDVI(:,i);
    [t1,t1_hat,t1_mu,t1_ci]=normfit(Y);            
    cdft1=[Y,normcdf(Y,t1)];                     
    [h1,p1]=kstest(Y,cdft1);h(1,:)=h1;p(1,:)=p1;   
    Y1=normcdf(Y,t1,t1_hat,t1_mu,t1_ci);           
    AllY(:,1) = Y1;
    rmse(1,:)=sqrt((Y1-V1(:,i))'*(Y1-V1(:,i))/length(Y1));   
     
    [t2,t2_ci]=gevfit(Y);
    cdft2=[Y,gevcdf(Y,t2(1),t2(2),t2(3))];
    [h2,p2]=kstest(Y,cdft2);h(2,:)=h2;p(2,:)=p2;
    Y2=gevcdf(Y,t2(1),t2(2),t2(3));
    AllY(:,2) = Y2;
    rmse(2,:)=sqrt((Y2-V1(:,i))'*(Y2-V1(:,i))/length(Y2));
     
    [t3, t3_ci] = expfit(Y);
    cdft3 = [Y, expcdf(Y, t3)];
    [h3, p3] = kstest(Y, cdft3);h(3,:)=h3;p(3,:)=p3;
    Y3 = expcdf(Y, t3);
    AllY(:,3) = Y3;
    rmse(3,:)=sqrt((Y3-V1(:,i))'*(Y3-V1(:,i))/length(Y));
     
    [t4, t4_ci] = lognfit(Y);
    cdft4 = [Y, logncdf(Y, t4(1), t4(2))];
    [h4, p4] = kstest(Y, cdft4);h(4,:)=h2;p(4,:)=p4;
    Y4 = logncdf(Y, t4(1), t4(2));
    AllY(:,4) = Y4;
    rmse(4,:)=sqrt((Y4-V1(:,i))'*(Y4-V1(:,i))/length(Y));
     
    [t5, t5_ci] = wblfit(Y);
    cdft5 = [Y, wblcdf(Y, t5(1), t5(2))];
    [h5, p5] = kstest(Y, cdft5);h(5,:)=h5;p(5,:)=p5;
    Y5 = wblcdf(Y, t5(1), t5(2));
    AllY(:,5) = Y5;
    rmse(5,:) = sqrt((Y5-V1(:,i))'*(Y5-V1(:,i))/length(Y));
    
    lambda = poissfit(Y);
    cdft6 = [Y, poisscdf(Y, lambda)];
    [h6, p6] = kstest(Y, cdft6);h(6,:)=h6;p(6,:)=p6;
    Y6 = poisscdf(Y, lambda);
    AllY(:,6) = Y6;
    rmse(6,:)=sqrt((Y6-V1(:,i))'*(Y6-V1(:,i))/length(Y));
     
    [parmhat, parmci] = gpfit(Y);
    cdft7 = [Y, gpcdf(Y, parmhat(1), parmhat(2))];
    [h7, p7] = kstest(Y, cdft7);h(7,:)=h7;p(7,:)=p7;
    Y7 = gpcdf(Y, parmhat(1), parmhat(2));
    AllY(:,7) = Y7;
    rmse(7,:)=sqrt((Y7-V1(:,i))'*(Y7-V1(:,i))/length(Y));
    
    [alpha , beta , a0] = fitP3( Y );
    cdft8 = [Y, P3cdf( Y , alpha , beta , a0 )];
    [h8, p8] = kstest(Y, cdft7);h(8,:)=h8;p(8,:)=p8;
    Y8 = P3cdf( Y , alpha , beta , a0 );
    AllY(:,8) = Y8;
    rmse(8,:)=sqrt((Y8-V1(:,i))'*(Y8-V1(:,i))/length(Y));
    
    AllRHP(:,1,1) = h;
    AllRHP(:,1,2) = p;
    AllRHP(:,1,3) = rmse;
    hpPosi = find(AllRHP(:,1,1) == 0 & AllRHP(:,1,2) > 0.05);  
    rPosi = find(AllRHP(hpPosi,1,3) == min(AllRHP(hpPosi,1,3))); 
    Posi = hpPosi(rPosi);
    if isempty(Posi)
        bestndvi(:,i) = AllY(:,2);  
        Nodistribution = [Nodistribution;i];
        AllPosi = [AllPosi,2];
    else
        AllPosi = [AllPosi,Posi];     
        bestndvi(:,i) = AllY(:,Posi);     
    end
end
V2 = bestndvi;
U2 = [];
for i = 1:EGS_SPI 
    X = bestSPI(:,i);
    [p3,p4] = normfit(X);
    U2(:,i) = normcdf(X,p3,p4);
end
% =============================Calculate the value matrices of copula functions=========================================================

Cga = []; 
for i = 1:EGS_SPI
    r1 = copulafit('Gaussian',[U2(:,i),V2(:,i)]);
    Cga(:,i) = copulacdf('Gaussian',[U2(:,i),V2(:,i)],r1);
end

Ct = []; 
for i = 1:EGS_SPI 
    [rho,nu]=copulafit('t',[U2(:,i),V2(:,i)]);
    Ct(:,i)=copulacdf('t',[U2(:,i),V2(:,i)],rho,nu);
end

Ccla = []; 
for i=1:EGS_SPI 
    pahat=copulafit('Clayton',[U2(:,i),V2(:,i)]);
    Ccla(:,i)=copulacdf('Clayton',[U2(:,i),V2(:,i)],pahat);
end

Cfr = []; 
for i=1:EGS_SPI 
    pahat=copulafit('Frank',[U2(:,i),V2(:,i)]);
    Cfr(:,i)=copulacdf('Frank',[U2(:,i),V2(:,i)],pahat);
end

Cgu = []; 
for i=1:EGS_SPI 
    pahat=copulafit('Gumbel',[U2(:,i),V2(:,i)]);
    Cgu(:,i)=copulacdf('Gumbel',[U2(:,i),V2(:,i)],pahat);
end
clear r1 cga rho nu ct ccla cfr cgu pahat i ii

dgu = []; dfr = dgu; dcla = dfr; dt = dcla; dga = dt;Dall = dga;
for i = 1:EGS_SPI
    dga(:,i)=(CUV(:,i)-Cga(:,i))'*(CUV(:,i)-Cga(:,i));
    dt(:,i)=(CUV(:,i)-Ct(:,i))'*(CUV(:,i)-Cga(:,i));
    dcla(:,i)=(CUV(:,i)-Ccla(:,i))'*(CUV(:,i)-Ccla(:,i));
    dfr(:,i)=(CUV(:,i)-Cfr(:,i))'*(CUV(:,i)-Cfr(:,i));
    dgu(:,i)=(CUV(:,i)-Cgu(:,i))'*(CUV(:,i)-Cgu(:,i));
    A = [dga(i),dt(i),dcla(i),dfr(i),dgu(i)];
    Dall(:,i) = find(A == min(A));
end
clearvars -except RENDVI RESPI Pmatrix bestP bestSPI PT U1 V1 U2 V2 CUV EGS_SPI maxR Dall AllPosi Y_SPI QQ shandiao

% ==============================Application in different scenarios========================================================
tic;
DD = [];
for i = 1:EGS_SPI
    if size(find(isnan(V2(:,i)) == 0),1) == 12
        Per = prctile(V2(:,i),30);
        if AllPosi(:,i) == 1
            [t1,t1_hat,t1_mu,t1_ci]=normfit(V2(:,i));
            scene_NDVI = normcdf(Per,t1,t1_hat,t1_mu,t1_ci);
        elseif AllPosi(:,i) == 2
            [t2,t2_ci] = gevfit(V2(:,i));
            scene_NDVI = gevcdf(Per,t2(1),t2(2),t2(3));
        elseif AllPosi(:,i) == 3
            [t3, t3_ci] = expfit(V2(:,i));
            scene_NDVI = expcdf(Per, t3);
        elseif AllPosi(:,i) == 4
            [t4, t4_ci] = lognfit(V2(:,i));
            scene_NDVI = logncdf(Per, t4(1), t4(2));
        elseif AllPosi(:,i) == 5
            [t5, t5_ci] = wblfit(V2(:,i));
            scene_NDVI = wblcdf(Per, t5(1), t5(2));
        elseif AllPosi(:,i) == 6
            lambda = poissfit(V2(:,i));
            scene_NDVI = poisscdf(Per, lambda);
        elseif AllPosi(:,i) == 7
            [parmhat, parmci] = gpfit(V2(:,i));
            scene_NDVI = gpcdf(Per, parmhat(1), parmhat(2));
        else
            [alpha , beta , a0] = fitP3( V2(:,i) );
            scene_NDVI = P3cdf( Per , alpha , beta , a0 );
        end
        
        [r,r_hat,r_mu,r_ci]=normfit(U2(:,i));
        scene_SPI(1,i)=normcdf(-1.5,r);
        scene_SPI(2,i)=normcdf(-2,r);
        
        if Dall(:,i) == 1
            r1 = copulafit('Gaussian',[U2(:,i),V2(:,i)]);
            scene(1,i) = copulacdf('Gaussian',[scene_SPI(1,i),scene_NDVI],r1);
            scene(2,i) = copulacdf('Gaussian',[scene_SPI(2,i),scene_NDVI],r1);
        elseif Dall(:,i) == 2
            [rho,nu] = copulafit('t',[U2(:,i),V2(:,i)]);
            scene(1,i) = copulacdf('t',[scene_SPI(1,i),scene_NDVI],rho,nu);
            scene(2,i) = copulacdf('t',[scene_SPI(2,i),scene_NDVI],rho,nu);
        elseif Dall(:,i) == 3
            pahat = copulafit('Clayton',[U2(:,i),V2(:,i)]);
            scene(1,i) = copulacdf('Clayton',[scene_SPI(1,i),scene_NDVI],pahat);
            scene(2,i) = copulacdf('Clayton',[scene_SPI(2,i),scene_NDVI],pahat);
        elseif Dall(:,i) == 4
            pahat = copulafit('Frank',[U2(:,i),V2(:,i)]);
            scene(1,i) = copulacdf('Frank',[scene_SPI(1,i),scene_NDVI],pahat);
            scene(2,i) = copulacdf('Frank',[scene_SPI(2,i),scene_NDVI],pahat);
        else
            pahat = copulafit('Gumbel',[U2(:,i),V2(:,i)]);
            scene(1,i) = copulacdf('Gumbel',[scene_SPI(1,i),scene_NDVI],pahat);
            scene(2,i) = copulacdf('Gumbel',[scene_SPI(2,i),scene_NDVI],pahat);
        end
        P_scenes(1,1) = ((scene(1,i))-(scene(2,i)))/((scene_SPI(1,i))-(scene_SPI(2,i)));
        DD = [DD,P_scenes];
        
 
        [r,r_hat,r_mu,r_ci]=normfit(U2(:,i));
        scene_SPI(1,i)=normcdf(-2,r);
        if Dall(:,i) == 1
            r1 = copulafit('Gaussian',[U2(:,i),V2(:,i)]);
            scene(1,i) = copulacdf('Gaussian',[scene_SPI(1,i),scene_NDVI],r1);
        elseif Dall(:,i) == 2
            [rho,nu] = copulafit('t',[U2(:,i),V2(:,i)]);
            scene(1,i) = copulacdf('t',[scene_SPI(1,i),scene_NDVI],rho,nu);
        elseif Dall(:,i) == 3
            pahat = copulafit('Clayton',[U2(:,i),V2(:,i)]);
            scene(1,i) = copulacdf('Clayton',[scene_SPI(1,i),scene_NDVI],pahat);
        elseif Dall(:,i) == 4
            pahat = copulafit('Frank',[U2(:,i),V2(:,i)]);
            scene(1,i) = copulacdf('Frank',[scene_SPI(1,i),scene_NDVI],pahat);
        else
            pahat = copulafit('Gumbel',[U2(:,i),V2(:,i)]);
            scene(1,i) = copulacdf('Gumbel',[scene_SPI(1,i),scene_NDVI],pahat);
        end
        P_scenes(1,1) = scene(1,i)/scene_SPI(1,i);
        DD = [DD,P_scenes];
        
    end
end
toc
clearvars -except RENDVI RESPI Pmatrix bestP bestSPI PT U1 V1 U2 V2 CUV EGS_SPI maxR Dall AllPosi Y_SPI DD ALLyuzhi MoveWinyuzhi QQ

time1 = toc;




