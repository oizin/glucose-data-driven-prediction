using Plots
using DifferentialEquations

function minimal_model!(du,u,p,t)
  G,X,I = u
  du[1] = dG = -G*(p1 + X) + p1*Gb
  du[2] = dX = -p2*X + p3*(I - Ib)
  du[3] = dI = -p4*(I - Ib) + p(t)
end

p1 = 0.1082
p2 = 0.02
p3 = 5.3*1e-6
p4 = 0.2659
Gb = 110
Ib = 90

u₀ = [1.2*Gb;0.0;1.2*Ib]
in = t->abs(100sin(10t))
plot(1:1:100,t->in(t))
tspan = (0.0,1000.0)
prob = ODEProblem(minimal_model!,u₀,tspan,in)
sol = solve(prob)
plot(sol.t,sol[1,:],ylims=(0,150))

#plot(sol.t,sol[2,:])
