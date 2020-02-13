function fintt,vx,vy,vz,bn

arg=bn(0) + bn(1)*vx   + bn(2)*vy   + bn(3)*vz   +$
            bn(4)*vx^2 + bn(5)*vy^2 + bn(6)*vz^2 +$
            bn(7)*vx*vy + bn(8)*vx*vz + bn(9)*vy*vz
return, exp(50.*arg)
end