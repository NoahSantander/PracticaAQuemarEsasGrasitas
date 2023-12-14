module Library where
import PdePreludat

-- Defino mis alias
type Edad = Number
type Peso = Number
type CoeficienteDeTonificacion = Number
type Persona = (Edad, Peso, CoeficienteDeTonificacion)
type Minutos = Number
type Ejercicio = Minutos -> Persona -> Persona
type Calorias = Number
type PesoPesas = Number
type Inclinacion = Number
type Nombre = String
type Duracion = Number
type Ejercicios = [Ejercicio]
type Rutina = (Nombre, Duracion, Ejercicios)
type KilosPerdidos = Number
type TonificacionGanada = Number
type ResumenRutina = (Nombre, KilosPerdidos, TonificacionGanada)
type Rutinas = [Rutina]

-- Defino mis tipos


-- Inicializo algunas personas
pancho = ( 40, 120, 1)
andres = ( 22, 80, 6)

-- Inicializo algunos ejercicios
relax :: Ejercicio
relax minutos persona = persona

-- Defino si alguien esta saludable
estaObeso :: Persona -> Bool
estaObeso (_, peso, _) = peso > 100

tieneMasDe :: Edad -> Persona -> Bool
tieneMasDe edadMaxima (edad, _, _) = edad > edadMaxima

saludable :: Persona -> Bool
saludable (edad, peso, coeficienteDeTonificacion) = (not.estaObeso) (edad, peso, coeficienteDeTonificacion) && (coeficienteDeTonificacion > 5)

-- Defino que una persona baje de peso en base a una cantidad de calorias quemadas
calcularPesoPerdido :: Calorias -> Persona -> Peso
calcularPesoPerdido calorias (edad, peso, coeficienteDeTonificacion) 
    | estaObeso (edad, peso, coeficienteDeTonificacion)  = calorias/150
    | (not.estaObeso) (edad, peso, coeficienteDeTonificacion) && tieneMasDe 30 (edad, peso, coeficienteDeTonificacion) && calorias > 200 = 1
    | otherwise = calorias/(peso*edad)

quemarCalorias :: Persona -> Calorias -> Persona
quemarCalorias (edad, peso, coeficienteDeTonificacion) calorias = (edad, (peso-) (calcularPesoPerdido calorias (edad, peso, coeficienteDeTonificacion)), coeficienteDeTonificacion)

-- Inicializo los ejercicios
tonificar :: CoeficienteDeTonificacion -> Persona -> Persona
tonificar coeficiente (edad, peso, coeficienteDeTonificacion) = (edad, peso, coeficienteDeTonificacion + coeficiente)

caminata :: Ejercicio
caminata minutos persona = quemarCalorias persona (minutos*5)

entrenamientoEnCinta :: Ejercicio
entrenamientoEnCinta minutos persona = quemarCalorias persona (minutos*(6+(minutos/5)/2))

pesas :: PesoPesas -> Ejercicio
pesas pesoPesas minutos persona 
    | minutos > 10 = tonificar (pesoPesas/10) persona
    | otherwise = persona

colina :: Inclinacion -> Ejercicio
colina inclinacion minutos persona = quemarCalorias persona (2*inclinacion*minutos)

montana :: Inclinacion -> Ejercicio
montana inclinacion minutos persona = tonificar 1 (((colina (3+inclinacion) (minutos/2)).(colina inclinacion (minutos/2))) persona)

-- Defino que una persona realice una rutina de ejercicios
realizarRutina :: Rutina -> Persona -> Persona
realizarRutina (_, duracion, ejercicios) persona = foldr ($duracion) persona $ ejercicios 

{-- realizarRutina :: Rutina -> Persona -> Persona
realizarRutina (_, duracion, [ejercicio]) persona = ejercicio duracion persona
realizarRutina (nombre, duracion, (ejercicio:ejercicios)) persona = realizarRutina (nombre, duracion, ejercicios) (ejercicio duracion persona) --}

mostrarResultados :: Rutina -> Persona -> Persona -> ResumenRutina
mostrarResultados (nombre, _, _) (_, pesoNuevo, tonificacionNueva) (_, peso, tonificacion) = (nombre, peso - pesoNuevo, tonificacion - tonificacionNueva)

resumenRutina :: Rutina -> Persona -> ResumenRutina
resumenRutina rutina persona = mostrarResultados rutina (realizarRutina rutina persona) persona

-- Defino las rutinas de una lista de rutinas que permiten volver saludable a una persona
rutinaSaludable :: Persona -> Rutina -> Bool
rutinaSaludable persona rutina = (saludable.realizarRutina rutina) persona

rutinasSaludables :: Rutinas -> Persona -> Rutinas
rutinasSaludables rutinas persona = filter (rutinaSaludable persona) rutinas