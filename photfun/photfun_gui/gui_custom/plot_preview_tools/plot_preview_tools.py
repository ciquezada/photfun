import os
import numpy as np
import imageio.v2 as imageio  # Asegura compatibilidad con versiones nuevas
from io import BytesIO
import base64
import matplotlib.pyplot as plt
from matplotlib.colors import LogNorm, Normalize
from PIL import Image
from scipy.ndimage import map_coordinates
from scipy.optimize import curve_fit
from concurrent.futures import ThreadPoolExecutor






def source_preview(row, fits_image, n_jobs=1):
    # Coordenadas de la posición (X, Y) de la fuente
    X_pos = float(row['X'])
    Y_pos = float(row['Y'])

    # Tamaño del recorte (cuadrado de 20x20 píxeles alrededor de la fuente)
    size = 20
    x_min = max(X_pos - size // 2, 0)
    x_max = min(X_pos + size // 2, fits_image.data.shape[1])
    y_min = max(Y_pos - size // 2, 0)
    y_max = min(Y_pos + size // 2, fits_image.data.shape[0])

    # Extraer el recorte de la imagen FITS
    image_data = np.array(fits_image.data)[int(y_min):int(y_max), int(x_min):int(x_max)]
    image_data = np.nan_to_num(image_data, nan=0)
    image_data[image_data <= 0] = 0.0001
    vmin, vmax = np.percentile(image_data, [2, 98])
    # image_data = np.log10(image_data)

    # Crear la figura para la animación
    fig, ax = plt.subplots(figsize=(2, 2), dpi=200)
    # vmin, vmax = np.percentile(image_data, [5, 95])
    ax.imshow(image_data, cmap='gray', norm=LogNorm(vmin=vmin, vmax=vmax))
    ax.invert_yaxis()

    # Guardar el frame de la animación
    buf = BytesIO()
    plt.savefig(buf, format="png", bbox_inches="tight")
    buf.seek(0)

    # Convertir el frame a GIF (solo un frame por ahora)
    frames = [imageio.imread(buf)]  # Solo un frame ya que no estamos animando más

    # Convertir los frames a GIF con loop infinito
    gif_buffer = BytesIO()
    imageio.mimsave(gif_buffer, frames, format="gif", duration=0.05, loop=0)  # loop=0 para repetir
    gif_buffer.seek(0)

    plt.close(fig)

    return base64.b64encode(gif_buffer.getvalue()).decode()

def generate_prof(row, fits_image, n_jobs=1):
    # Coordenadas de la posición (X, Y) de la fuente
    X_pos = float(row['X'])
    Y_pos = float(row['Y'])

    # Tamaño del recorte (cuadrado de 20x20 píxeles alrededor de la fuente)
    size = 30
    x_min = max(X_pos - size // 2, 0)
    x_max = min(X_pos + size // 2, fits_image.data.shape[1])
    y_min = max(Y_pos - size // 2, 0)
    y_max = min(Y_pos + size // 2, fits_image.data.shape[0])

    # Extraer el recorte de la imagen FITS
    image_data = np.array(fits_image.data)[int(y_min):int(y_max), int(x_min):int(x_max)]
    image_data = np.nan_to_num(image_data, nan=0)
    image_data[image_data <= 0] = 0.00001
    vmin, vmax = np.percentile(image_data, [5, 95])
    image_data = np.log10(image_data)
    image_data[~np.isfinite(image_data)] = vmin 

    img_width, img_height = 300, 300  # Tamaño fijo para todas las imágenes de la animación

    angles = np.linspace(0, 180, num=90)  # 30 ángulos
    
    # Función para procesar cada ángulo en paralelo
    def process_angle(angle):
        theta = np.deg2rad(angle)
        length = size // 2
        x1, y1 = X_pos + length * np.cos(theta), Y_pos + length * np.sin(theta)
        x2, y2 = X_pos - length * np.cos(theta), Y_pos - length * np.sin(theta)
        
        num_points = 150
        x_vals = np.linspace(x1, x2, num_points)
        y_vals = np.linspace(y1, y2, num_points)
        
        line_values = map_coordinates(image_data, [y_vals - y_min, x_vals - x_min], order=1, mode='nearest')

        # Alineación por máximo central
        N = len(line_values)
        center_mask = N // 2
        start_mask = center_mask - 10
        end_mask = start_mask + 20
        mask = line_values[start_mask:end_mask]
        max_mask = np.argmax(mask)
        indx_max = start_mask + max_mask
        shift = center_mask - indx_max
        return np.roll(line_values, shift)[25:125]

    # Procesamiento paralelo con ThreadPoolExecutor
    with ThreadPoolExecutor(max_workers=os.cpu_count() if n_jobs==-1 else n_jobs) as executor:
        all_profiles = list(executor.map(process_angle, angles))

    # Configurar figura y graficar resultados
    fig, axs = plt.subplots(1, 1, figsize=(2, 2), dpi=200)
    for line_values in all_profiles:
        axs.plot(line_values, color='gray', alpha=0.7, lw=0.5)
    
    # Calcular perfil promedio
    mean_profile = np.mean(all_profiles, axis=0)
    axs.plot(mean_profile, color='red', lw=0.5, label='Promedio')
    y_min_plot, y_max_plot = np.nanpercentile(mean_profile, [0, 95])
    axs.set_ylim(y_min_plot, y_max_plot * 1.3)
    axs.set_xlim(0, 100)
    plt.tight_layout()

    # Generar GIF
    buf = BytesIO()
    plt.savefig(buf, format="png", bbox_inches="tight")
    buf.seek(0)
    frames = [imageio.imread(buf)]
    gif_buffer = BytesIO()
    imageio.mimsave(gif_buffer, frames, format="gif", duration=0.05, loop=0)
    gif_buffer.seek(0)
    plt.close(fig)

    return base64.b64encode(gif_buffer.getvalue()).decode()

def generate_prof_fast(row, fits_image, n_jobs=1):
    # Coordenadas de la posición (X, Y) de la fuente
    X_pos = float(row['X'])
    Y_pos = float(row['Y'])

    # Tamaño del recorte (cuadrado de 20x20 píxeles alrededor de la fuente)
    size = 30
    x_min = max(X_pos - size // 2, 0)
    x_max = min(X_pos + size // 2, fits_image.data.shape[1])
    y_min = max(Y_pos - size // 2, 0)
    y_max = min(Y_pos + size // 2, fits_image.data.shape[0])

    # Extraer el recorte de la imagen FITS
    image_data = np.array(fits_image.data)[int(y_min):int(y_max), int(x_min):int(x_max)]
    image_data = np.nan_to_num(image_data, nan=0)
    image_data[image_data <= 0] = 0.00001
    vmin, vmax = np.percentile(image_data, [5, 95])
    image_data = np.log10(image_data)
    image_data[~np.isfinite(image_data)] = vmin 

    img_width, img_height = 300, 300  # Tamaño fijo para todas las imágenes de la animación

    angles = np.linspace(0, 180, num=30)  # 30 ángulos
    
    # Función para procesar cada ángulo en paralelo
    def process_angle(angle):
        theta = np.deg2rad(angle)
        length = size // 2
        x1, y1 = X_pos + length * np.cos(theta), Y_pos + length * np.sin(theta)
        x2, y2 = X_pos - length * np.cos(theta), Y_pos - length * np.sin(theta)
        
        num_points = 150
        x_vals = np.linspace(x1, x2, num_points)
        y_vals = np.linspace(y1, y2, num_points)
        
        line_values = map_coordinates(image_data, [y_vals - y_min, x_vals - x_min], order=1, mode='nearest')

        # Alineación por máximo central
        N = len(line_values)
        center_mask = N // 2
        start_mask = center_mask - 10
        end_mask = start_mask + 20
        mask = line_values[start_mask:end_mask]
        max_mask = np.argmax(mask)
        indx_max = start_mask + max_mask
        shift = center_mask - indx_max
        return np.roll(line_values, shift)[25:125]

    # Procesamiento paralelo con ThreadPoolExecutor
    with ThreadPoolExecutor(max_workers=os.cpu_count() if n_jobs==-1 else n_jobs) as executor:
        all_profiles = list(executor.map(process_angle, angles))

    # Configurar figura y graficar resultados
    fig, axs = plt.subplots(1, 1, figsize=(2, 2), dpi=200)
    for line_values in all_profiles:
        axs.plot(line_values, color='gray', alpha=0.7, lw=0.5)
    
    # Calcular perfil promedio
    mean_profile = np.mean(all_profiles, axis=0)
    axs.plot(mean_profile, color='red', lw=0.5, label='Promedio')
    y_min_plot, y_max_plot = np.nanpercentile(mean_profile, [0, 95])
    axs.set_ylim(y_min_plot, y_max_plot * 1.3)
    axs.set_xlim(0, 100)
    plt.tight_layout()

    # Generar GIF
    buf = BytesIO()
    plt.savefig(buf, format="png", bbox_inches="tight")
    buf.seek(0)
    frames = [imageio.imread(buf)]
    gif_buffer = BytesIO()
    imageio.mimsave(gif_buffer, frames, format="gif", duration=0.05, loop=0)
    gif_buffer.seek(0)
    plt.close(fig)

    return base64.b64encode(gif_buffer.getvalue()).decode()

def generate_prof_animation(row, fits_image, n_jobs=1):
    # Coordenadas de la posición (X, Y) de la fuente
    X_pos = float(row['X'])
    Y_pos = float(row['Y'])

    # Tamaño del recorte (cuadrado de 20x20 píxeles alrededor de la fuente)
    size = 20
    x_min = max(X_pos - size // 2, 0)
    x_max = min(X_pos + size // 2, fits_image.data.shape[1])
    y_min = max(Y_pos - size // 2, 0)
    y_max = min(Y_pos + size // 2, fits_image.data.shape[0])

    # Extraer el recorte de la imagen FITS
    image_data = np.array(fits_image.data)[int(y_min):int(y_max), int(x_min):int(x_max)]
    image_data = np.nan_to_num(image_data, nan=0)
    image_data[image_data <= 0] = 0.0001
    vmin, vmax = np.percentile(image_data, [5, 95])
    image_data = np.log10(image_data)
    image_data[~np.isfinite(image_data)] = vmin 
    y_min_plot, y_max_plot = np.nanpercentile(image_data, [5, 95])

    img_width, img_height = 300, 300  # Tamaño fijo para todas las imágenes de la animación

    frames = []
    angles = np.linspace(0, 180, num=10)  # 36 pasos (cada 5 grados)
    
    for angle in angles:
        fig, axs = plt.subplots(1, 1, figsize=(2, 2), dpi=200)
        
        # Coordenadas de la línea
        length = size // 2
        theta = np.deg2rad(angle)
        x1, y1 = X_pos + length * np.cos(theta), Y_pos + length * np.sin(theta)
        x2, y2 = X_pos - length * np.cos(theta), Y_pos - length * np.sin(theta)
        
        # Puntos de la línea
        num_points = 100
        x_vals = np.linspace(x1, x2, num_points)
        y_vals = np.linspace(y1, y2, num_points)
        
        # Extraer valores de la imagen a lo largo de la línea
        line_values = map_coordinates(image_data, [y_vals - y_min, x_vals - x_min], order=1, mode='nearest')

        
        # Graficar la intensidad a lo largo de la línea
        axs.plot(line_values, "r")
        axs.set_ylim(y_min_plot, y_max_plot * 1.5)
        axs.set_xlim(0, 100)
        # axs.set_title("Intensity Profile")
        # axs.set_xlabel("Position along line")
        # axs.set_ylabel("Intensity")
        plt.tight_layout()
        
        # Guardar el frame
        buf = BytesIO()
        plt.savefig(buf, format="png", bbox_inches="tight")
        buf.seek(0)
        # Leer la imagen y redimensionarla a un tamaño fijo
        img = imageio.imread(buf)
        img_resized = np.array(Image.fromarray(img).resize((img_width, img_height)))  # Redimensionar la imagen
        frames.append(img_resized)
        plt.close(fig)
    
    # Convertir los frames a GIF con loop infinito
    gif_buffer = BytesIO()
    imageio.mimsave(gif_buffer, frames, format="gif", duration=0.1, loop=0)
    gif_buffer.seek(0)
    
    return base64.b64encode(gif_buffer.getvalue()).decode()
                # # AQUI PARTE LA ANIMACION
        # point, = ax.plot([], [], "ro", markersize=6)

        # frames = []
        # for i in range(0, len(x), 10):  # Avanza 10 pasos por frame
        #     point.set_data([x[i]], [y[i]])
        #     buf = BytesIO()
        #     plt.savefig(buf, format="png", bbox_inches="tight")
        #     buf.seek(0)
        #     frames.append(imageio.imread(buf))  # Leer la imagen en memoria
        
        # plt.close(fig)

def generate_rotation_animation(row, fits_image, n_jobs=1):
    # Coordenadas de la posición (X, Y) de la fuente
    X_pos = float(row['X'])
    Y_pos = float(row['Y'])

    # Tamaño del recorte (cuadrado de 20x20 píxeles alrededor de la fuente)
    size = 20
    x_min = max(X_pos - size // 2, 0)
    x_max = min(X_pos + size // 2, fits_image.data.shape[1])
    y_min = max(Y_pos - size // 2, 0)
    y_max = min(Y_pos + size // 2, fits_image.data.shape[0])

    # Extraer el recorte de la imagen FITS
    image_data = np.array(fits_image.data)[int(y_min):int(y_max), int(x_min):int(x_max)]
    image_data = np.nan_to_num(image_data, nan=0)
    image_data[image_data <= 0] = 0.0001
    vmin, vmax = np.percentile(image_data, [5, 95])

    # Crear la figura para la proyección 3D
    fig_3d = plt.figure(figsize=(2, 2), dpi=150)  # Reducir la resolución para mejorar rendimiento
    graph_ax = fig_3d.add_subplot(111, projection='3d')

    # Crear malla de coordenadas (X, Y)
    X_data, Y_data = np.meshgrid(np.arange(image_data.shape[1]), np.arange(image_data.shape[0]))
    Z_data = image_data  # Usamos la imagen FITS como los valores Z

    # Graficar superficie 3D
    surf = graph_ax.plot_surface(X_data, Y_data, np.log10(Z_data), cmap='inferno', edgecolor='none')
    # graph_ax.set_ylabel("Y (PIX)")
    # graph_ax.set_xlabel("X (PIX)")

    # Eliminar los ticks
    graph_ax.set_xticks([])
    graph_ax.set_yticks([])
    graph_ax.set_zticks([])


    # Definir el tamaño de la imagen de salida
    img_width, img_height = 300, 300  # Tamaño fijo para todas las imágenes de la animación
    

    # Crear animación de rotación
    frames = []
    for angle in range(0, 180, 30):  # Rotar en pasos de 5 grados
        graph_ax.view_init(azim=angle, elev=20.)  # Cambiar el ángulo de vista

        # Guardar el frame de la animación
        buf_3d = BytesIO()
        plt.savefig(buf_3d, format="png", bbox_inches="tight", dpi=150)  # Reducir la resolución para mejorar rendimiento
        buf_3d.seek(0)

        # Leer la imagen y redimensionarla a un tamaño fijo
        img = imageio.imread(buf_3d)
        img_resized = np.array(Image.fromarray(img).resize((img_width, img_height)))  # Redimensionar la imagen

        frames.append(img_resized)  # Agregar el frame redimensionado

    frames += frames[::-1]

    # Convertir los frames a GIF con rotación continua
    gif_buffer = BytesIO()
    imageio.mimsave(gif_buffer, frames, format="gif", duration=0.2, loop=0)  # `loop=0` para repetir
    gif_buffer.seek(0)

    plt.close(fig_3d)

    return base64.b64encode(gif_buffer.getvalue()).decode()



# def gaussian_2d(xy, A, x0, y0, sigma_x, sigma_y, theta, offset):
#     """ Función de una Gaussiana 2D rotada. """
#     x, y = xy
#     x0, y0 = float(x0), float(y0)
#     a = (np.cos(theta) ** 2) / (2 * sigma_x ** 2) + (np.sin(theta) ** 2) / (2 * sigma_y ** 2)
#     b = -(np.sin(2 * theta)) / (4 * sigma_x ** 2) + (np.sin(2 * theta)) / (4 * sigma_y ** 2)
#     c = (np.sin(theta) ** 2) / (2 * sigma_x ** 2) + (np.cos(theta) ** 2) / (2 * sigma_y ** 2)
#     return A * np.exp(-(a * (x - x0) ** 2 + 2 * b * (x - x0) * (y - y0) + c * (y - y0) ** 2)) + offset

# def source_preview_fit(row, fits_image):
#     """ Ajusta una Gaussiana 2D a la imagen en un radio de 4 píxeles del centro y muestra los residuales. """
#     # Coordenadas de la posición (X, Y) de la fuente
#     X_pos = int(row['X'])
#     Y_pos = int(row['Y'])

#     # Tamaño del recorte (cuadrado de 20x20 píxeles alrededor de la fuente)
#     radius = 10
#     x_min = max(X_pos - radius // 2, 0)
#     x_max = min(X_pos + radius // 2, fits_image.data.shape[1])
#     y_min = max(Y_pos - radius // 2, 0)
#     y_max = min(Y_pos + radius // 2, fits_image.data.shape[0])

#     # Extraer el recorte de la imagen FITS
#     image_data = np.array(fits_image.data)[int(y_min):int(y_max), int(x_min):int(x_max)]
    
#     # Definir el radio de ajuste

#     # Extraer el recorte y transformar en logaritmo
#     image_data = np.array(fits_image.data)[int(y_min):int(y_max), int(x_min):int(x_max)]
#     vmin, vmax = np.percentile(image_data, [5, 99])
#     image_data = np.nan_to_num(image_data, nan=vmin, posinf=vmax, neginf=vmin)
#     image_data[image_data < vmin] = vmin


#     # Crear la grilla de coordenadas
#     y_grid, x_grid = np.meshgrid(np.arange(y_min, y_max), np.arange(x_min, x_max), indexing="ij")
    
#     # Datos en 1D para el ajuste
#     x_flat, y_flat, z_flat = x_grid.ravel(), y_grid.ravel(), image_data.ravel()
#     valid_mask = np.isfinite(z_flat)  # Más eficiente que np.isnan() + np.isinf()
#     x_flat = x_flat[valid_mask]
#     y_flat = y_flat[valid_mask]
#     z_flat = z_flat[valid_mask]

#     # Parámetros iniciales
#     initial_guess = [z_flat.max(), X_pos, Y_pos, 2, 2, 0, np.median(z_flat)]  # A, x0, y0, sigma_x, sigma_y, theta, offset

#     # Ajuste de la gaussiana
#     try:
#         popt, _ = curve_fit(gaussian_2d, (x_flat, y_flat), z_flat, p0=initial_guess)
#     except RuntimeError:
#         print("Error: No se pudo ajustar la Gaussiana")
#         return

#     #####################
#     # Tamaño del recorte (cuadrado de 20x20 píxeles alrededor de la fuente)
#     radius = 20
#     x_min = max(X_pos - radius // 2, 0)
#     x_max = min(X_pos + radius // 2, fits_image.data.shape[1])
#     y_min = max(Y_pos - radius // 2, 0)
#     y_max = min(Y_pos + radius // 2, fits_image.data.shape[0])

#     # Extraer el recorte de la imagen FITS
#     image_data = np.array(fits_image.data)[int(y_min):int(y_max), int(x_min):int(x_max)]
    
#     # Definir el radio de ajuste

#     # Extraer el recorte y transformar en logaritmo
#     image_data = np.array(fits_image.data)[int(y_min):int(y_max), int(x_min):int(x_max)]
#     vmin, vmax = np.percentile(image_data, [5, 95])
#     image_data = np.nan_to_num(image_data, nan=vmin, posinf=vmax, neginf=vmin)
#     image_data[image_data < vmin] = vmin


#     # Crear la grilla de coordenadas
#     y_grid, x_grid = np.meshgrid(np.arange(y_min, y_max), np.arange(x_min, x_max), indexing="ij")
    
#     # Crear la imagen ajustada
#     fitted_surface = gaussian_2d((x_grid, y_grid), *popt).reshape(image_data.shape)

#     # Calcular residuales
#     residuals = np.log10(image_data) - np.log10(fitted_surface)

#     # Crear la figura para la animación
#     fig, ax = plt.subplots(figsize=(2, 2), dpi=200)
#     vmin, vmax = np.percentile(image_data, [5, 95])
#     ax.imshow(residuals, cmap='coolwarm', origin="lower")
#     # ax.invert_yaxis()

#     # Guardar el frame de la animación
#     buf = BytesIO()
#     plt.savefig(buf, format="png", bbox_inches="tight")
#     buf.seek(0)

#     # Convertir el frame a GIF (solo un frame por ahora)
#     frames = [imageio.imread(buf)]  # Solo un frame ya que no estamos animando más

#     # Convertir los frames a GIF con loop infinito
#     gif_buffer = BytesIO()
#     imageio.mimsave(gif_buffer, frames, format="gif", duration=0.05, loop=0)  # loop=0 para repetir
#     gif_buffer.seek(0)

#     plt.close(fig)

#     return base64.b64encode(gif_buffer.getvalue()).decode()



# def generate_animation(row, fits_image):
#     # Coordenadas de la posición (X, Y) de la fuente
#     X_pos = int(row['X'])
#     Y_pos = int(row['Y'])

#     # Tamaño del recorte (cuadrado de 20x20 píxeles alrededor de la fuente)
#     size = 20
#     x_min = max(X_pos - size // 2, 0)
#     x_max = min(X_pos + size // 2, fits_image.data.shape[1])
#     y_min = max(Y_pos - size // 2, 0)
#     y_max = min(Y_pos + size // 2, fits_image.data.shape[0])

#     # Extraer el recorte de la imagen FITS
#     image_data = np.array(fits_image.data)[int(y_min):int(y_max), int(x_min):int(x_max)]
#     image_data = np.nan_to_num(image_data, nan=0)
#     image_data[image_data < 0] = 0


#     frames = []
#     angles = np.linspace(0, 180, num=180)  # 36 pasos (cada 5 grados)
    
#     for angle in angles:
#         fig, axs = plt.subplots(1, 2, figsize=(4, 2), dpi=200)
#         vmin, vmax = np.percentile(image_data, [5, 95])
#         axs[0].imshow(image_data, cmap='gray', norm=Normalize(vmin=vmin, vmax=vmax))
#         axs[0].invert_yaxis()
#         axs[0].set_title(f"Angle: {angle:.1f}°")
        
#         # Coordenadas de la línea
#         length = size // 2
#         theta = np.deg2rad(angle)
#         x1, y1 = X_pos + length * np.cos(theta), Y_pos + length * np.sin(theta)
#         x2, y2 = X_pos - length * np.cos(theta), Y_pos - length * np.sin(theta)
        
#         # Puntos de la línea
#         num_points = 100
#         x_vals = np.linspace(x1, x2, num_points)
#         y_vals = np.linspace(y1, y2, num_points)
        
#         # Extraer valores de la imagen a lo largo de la línea
#         line_values = map_coordinates(image_data, [y_vals - y_min, x_vals - x_min], order=1, mode='nearest')
        
#         # Dibujar la línea en la imagen
#         axs[0].plot([x1 - x_min, x2 - x_min], [y1 - y_min, y2 - y_min], 'r-', lw=1)
        
#         # Graficar la intensidad a lo largo de la línea
#         axs[1].plot(line_values, 'r-')
#         axs[1].set_title("Intensity Profile")
#         axs[1].set_xlabel("Position along line")
#         axs[1].set_ylabel("Intensity")
        
#         # Guardar el frame
#         buf = BytesIO()
#         plt.savefig(buf, format="png", bbox_inches="tight")
#         buf.seek(0)
#         frames.append(imageio.imread(buf))
#         plt.close(fig)
    
#     # Convertir los frames a GIF con loop infinito
#     gif_buffer = BytesIO()
#     imageio.mimsave(gif_buffer, frames, format="gif", duration=0.1, loop=0)
#     gif_buffer.seek(0)
    
#     return base64.b64encode(gif_buffer.getvalue()).decode()

# def generate_rotation_animation(row, fits_image, n_jobs=1):
#     # Coordenadas y preparación de datos
#     X_pos = float(row['X'])
#     Y_pos = float(row['Y'])
#     size = 20
    
#     # Recorte de la imagen (igual que antes)
#     x_min = max(X_pos - size // 2, 0)
#     x_max = min(X_pos + size // 2, fits_image.data.shape[1])
#     y_min = max(Y_pos - size // 2, 0)
#     y_max = min(Y_pos + size // 2, fits_image.data.shape[0])
    
#     image_data = np.array(fits_image.data)[int(y_min):int(y_max), int(x_min):int(x_max)]
#     image_data = np.nan_to_num(image_data, nan=0)
#     image_data[image_data <= 0] = 0.0001
#     vmin, vmax = np.percentile(image_data, [5, 95])
#     img_width, img_height = 300, 300

#     # Función para generar cada frame en paralelo
#     def generate_frame(angle):
#         fig = plt.figure(figsize=(2, 2), dpi=150)
#         ax = fig.add_subplot(111, projection='3d')
#         X, Y = np.meshgrid(np.arange(image_data.shape[1]), np.arange(image_data.shape[0]))
        
#         ax.plot_surface(X, Y, np.log10(image_data), cmap='inferno', edgecolor='none')
#         ax.view_init(azim=angle, elev=20)
#         ax.set_xticks([])
#         ax.set_yticks([])
#         ax.set_zticks([])
        
#         buf = BytesIO()
#         plt.savefig(buf, format="png", bbox_inches="tight", dpi=150)
#         plt.close(fig)
#         buf.seek(0)
        
#         img = imageio.imread(buf)
#         return np.array(Image.fromarray(img).resize((img_width, img_height))), angle

#     # Procesamiento paralelo de frames
#     angles = list(np.linspace(0, 180, 30))
#     # Procesamiento paralelo con ThreadPoolExecutor
#     with ThreadPoolExecutor(max_workers=os.cpu_count() if n_jobs==-1 else n_jobs) as executor:
#         frames = list(executor.map(generate_frame, angles))
#         frames = sorted(frames, key=lambda x: x[1])
#         frames = [fr[0] for fr in frames]
    
#     # Crear animación con rebote
#     frames += frames[::-1]
    
#     # Generar GIF
#     gif_buffer = BytesIO()
#     imageio.mimsave(gif_buffer, frames, format="gif", duration=0.2, loop=0)
#     gif_buffer.seek(0)

#     return base64.b64encode(gif_buffer.getvalue()).decode()