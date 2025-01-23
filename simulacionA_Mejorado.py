import simpy
import random
from scipy.stats import truncnorm

# Función auxiliar para generar valores truncados de una distribución normal
def truncated_normal(mean, std_dev, lower=0, upper=float('inf')):
    return truncnorm((lower - mean) / std_dev, (upper - mean) / std_dev, loc=mean, scale=std_dev).rvs()

# Función FNC1 basada en los datos de GPSS
def FNC1():
    probabilities = [0.59, 0.7026, 0.837, 0.864, 0.9728, 1.0]
    values = [0, 0.28, 1.3, 1.9, 2.57, 3.17]
    rnd = random.random()
    for i, p in enumerate(probabilities):
        if rnd <= p:
            return values[i]
    return 4.22

# Proceso de la simulación con estadísticas
def process_fnc1(env, resource, queue, stats):
    while True:
        arrival_time = env.now
        # Generar tiempo entre llegadas usando FNC1
        yield env.timeout(FNC1())
        
        # Entrar en la cola
        queue.append(env.now)
        with resource.request() as req:
            yield req  # Esperar hasta obtener acceso al recurso
            
            # Salir de la cola
            queue_time = env.now - arrival_time
            queue.pop(0)
            
            # Tiempo de servicio (normal truncada con menor desviación estándar)
            service_time = truncated_normal(1.2, 0.3)  # Desviación estándar reducida
            yield env.timeout(service_time)
            
            # Registrar estadísticas
            stats['times_in_queue'].append(queue_time)
            stats['service_times'].append(service_time)
            stats['system_times'].append(env.now - arrival_time)

def process_exponential(env, resource, queue, stats):
    while True:
        arrival_time = env.now
        # Generar tiempo entre llegadas usando distribución exponencial
        yield env.timeout(random.expovariate(1 / 1.44))  # Lambda = 1/1.44
        
        # Entrar en la cola
        queue.append(env.now)
        with resource.request() as req:
            yield req  # Esperar hasta obtener acceso al recurso
            
            # Salir de la cola
            queue_time = env.now - arrival_time
            queue.pop(0)
            
            # Tiempo de servicio (normal truncada con menor desviación estándar)
            service_time = truncated_normal(2, 0.6)  # Desviación estándar reducida
            yield env.timeout(service_time)
            
            # Registrar estadísticas
            stats['times_in_queue'].append(queue_time)
            stats['service_times'].append(service_time)
            stats['system_times'].append(env.now - arrival_time)

# Configuración principal del modelo
def run_simulation(duration):
    # Crear el entorno de simulación
    env = simpy.Environment()
    
    # Crear un recurso con capacidad incrementada (de 2 a 3)
    resource = simpy.Resource(env, capacity=3)
    
    # Cola para medir tiempos
    queue = []
    
    # Diccionario para recopilar estadísticas
    stats = {
        'times_in_queue': [],
        'service_times': [],
        'system_times': []
    }
    
    # Iniciar procesos
    env.process(process_fnc1(env, resource, queue, stats))
    env.process(process_exponential(env, resource, queue, stats))
    
    # Ejecutar la simulación
    env.run(until=duration)
    
    # Generar informe al final
    print("Resultados de la simulación (Mejoras aplicadas):")
    print(f"Tiempo promedio en cola: {sum(stats['times_in_queue']) / len(stats['times_in_queue']):.2f} minutos")
    print(f"Tiempo promedio de servicio: {sum(stats['service_times']) / len(stats['service_times']):.2f} minutos")
    print(f"Tiempo promedio en el sistema: {sum(stats['system_times']) / len(stats['system_times']):.2f} minutos")
    print(f"Número total de entidades procesadas: {len(stats['system_times'])}")

# Ejecutar la simulación por 120 minutos
run_simulation(120)