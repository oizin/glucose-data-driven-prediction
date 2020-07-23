using Plots
using DifferentialEquations

# Insulin glucose model from Lin, Lee et al (2008)
lin! = @ode_def LinLee begin
    dG = -pg*G - Si*(G + Ge)*(Q/(1 + αg*Q)) + P
    dQ = -k*Q + k*I
    dI = -n*(I/(1 + αi*I)) + uex/Vi
end G Q I
