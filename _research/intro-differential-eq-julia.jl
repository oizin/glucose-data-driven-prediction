using DifferentialEquations
using Plots

# exponential growth model
f(u,p,t) = 0.98u
u0 = 1.0
tspan = (0.0,1.0)
prob = ODEProblem(f,u0,tspan)

# solve the problem
sol = solve(prob)
println(sol)

# plots recipe
plot(sol)

# the result is a function
sol(0.45)

# a more accurate solution
sol = solve(prob,abstol=1e-8,reltol=1e-8)
plot(sol)
plot!(sol.t,t->1.0exp(0.98t),lwd=3,ls=:dash)

# save a particular values
sol = solve(prob,saveat=0.1)
sol = solve(prob,saveat=[0.1,0.5])

# choose algorithm
sol = solve(prob,alg_hints=[:stiff])

# explicity choose algorithm
sol = solve(prob,Tsit5(),reltol=1e-5)

# systems of ODES - Lorenz
function lorenz!(du,u,p,t)
    σ,ρ,β = p
    du[1] = σ*(u[2]-u[1])
    du[2] = u[1]*(ρ-u[3])-u[2]
    du[3] = u[1]*u[2] - β*u[3]
end

u0 = [1.0;0.0;0.0]
p = (10,28,8/3)
tspan = (0.0,100.0)
prob = ODEProblem(lorenz!,u0,tspan,p)
sol = solve(prob)
sol.t[10],sol[10]
# get parameter 2 at time 10
sol[2,10]
# convery to matrix
A = convert(Array,sol)
A
# time series
plot(sol)
# phase plot (lorenz attractor plot)
plot(sol,vars=(1,2,3))
plot(sol,vars=(1,2,3),denseplot=false)

# more complex systems (notation)
function lotka_volterra!(du,u,p,t)
    du[1] = p[1]*u[1] - p[2]*u[1]*u[2]
    du[2] = -p[3]*u[3] + p[4]*u[1]*u[2]
end
# or as...
lv! = @ode_def LotkaVolterra begin
    dx = a*x - b*x*y
    dy = -c*y + d*x*y
end a b c d
u0 = [1.0,1.0]
p = (1.5,1.0,3.0,1.0)
tspan = (0.0,10.0)
prob = ODEProblem(lv!,u0,tspan,p)
sol = solve(prob)
plot(sol)

using Latexify
latexify(lv!)
# look into parameterised functions
# look into static arrays
