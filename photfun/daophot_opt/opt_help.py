# Strings descriptivos
opt_daophot_labels = {
    're': 'Readout Noise',
    'ga': 'Gain',
    'lo': 'Low Good',
    'hi': 'High Good',
    'fw': 'FWHM of Star',
    'th': 'Threshold',
    'ls': 'Low Sharpness',
    'lr': 'Low Roundness',
    'hs': 'High Sharpness',
    'hr': 'High Roundness',
    'wa': 'Watch Progress',
    'fi': 'Fitting Radius',
    'ps': 'PSF Radius',
    'va': 'Variable PSF',
    'an': 'Analytic PSF',
    'ex': 'PSF Cleaning',
    'us': 'us (DEPRECATED)',
    'pr': 'Profile Error',
    'pe': 'Percent Error',
}

opt_photo_labels = {
    "A1": "Apert Radii 1",
    "A2": "Apert Radii 2",
    "A3": "Apert Radii 3",
    "A4": "Apert Radii 4",
    "A5": "Apert Radii 5",
    "A6": "Apert Radii 6",
    "A7": "Apert Radii 7",
    "A8": "Apert Radii 8",
    "A9": "Apert Radii 9",
    "AA": "Apert Radii 10",
    "AB": "Apert Radii 11",
    "AC": "Apert Radii 12",
    "IS": "InnerSky Radii",
    "OS": "OuterSky Radii"
}

opt_allstar_labels = {
    "fi": "Fitting Radius",
    "re": "Recenter Stars",
    "wa": "Watch Progress",
    "pe": "Percent Error",
    "ce": "Clip Exponent",
    "cr": "Clip Range",
    "ma": "Max Group Size",
    "pr": "Profile Error",
    "is": "InnerSky Radii",
    "os": "OuterSky Radii"
}

info_docstrings = {
"INFO_re": """
READ NOISE: 
El ruido de lectura, en números de datos (DN), de una sola exposición 
realizada con tu detector. Más tarde, el software permitirá especificar 
si un marco de datos es la suma o el promedio de múltiples exposiciones. 
Si tienes un subdirectorio separado para datos de una noche o corrida 
de observación, el ruido de lectura debe especificarse solo una vez 
(en el archivo DAOPHOT.OPT).
""",

"INFO_ga": """
GAIN: 
El factor de ganancia de tu detector, en fotones o electrones por número 
de datos (DN). Al igual que con el ruido de lectura, debes especificar 
la ganancia correspondiente a una sola exposición. Se puede hacer un 
ajuste más tarde para los marcos que son la media o la suma de varias 
exposiciones. Los valores por defecto son deliberadamente inválidos. 
Debes ingresar los valores correctos en un archivo DAOPHOT.OPT o el 
programa te pedirá que lo hagas repetidamente.
""",

"INFO_lo": """
LOW GOOD DATUM: 
El nivel, en desviaciones estándar por debajo del valor medio del cielo 
en el marco, por debajo del cual el programa considera un píxel como 
defectuoso. Si el fondo es plano en todo el marco, puedes establecer 
un límite ajustado, como 5σ. Si hay un gradiente de fondo fuerte, 
necesitarás un límite más generoso, tal vez 10σ o más, para evitar 
rechazar píxeles de cielo legítimos en las partes del marco donde el 
fondo es débil. El uso inteligente del comando DUMP y/o una visualización 
de imagen te ayudará a decidir.
""",

"INFO_hi": """
HIGH GOOD DATUM: 
El nivel, en números de datos (DN), por encima del cual un valor de 
píxel se considera defectuoso. Esto difiere del "LOW GOOD DATUM". 
El "LOW GOOD DATUM" se define como un cierto número de desviaciones 
estándar por debajo del valor medio del cielo del marco, por lo que 
necesita especificarse solo una vez si todos tus marcos tienen fondos 
comparables. El "HIGH GOOD DATUM" se especifica como un único número 
fijo que representa el nivel absoluto en números de datos donde el 
detector se vuelve no lineal o se satura. Como tus datos han sido 
sustrayendo el nivel de sesgo y normalizados, este número no será 32767, 
sino algo más bajo.
""",

"INFO_fw": """
FWHM: 
El FWHM aproximado, en píxeles, de los objetos para los cuales el 
algoritmo FIND está optimizado. Este parámetro determina el ancho de 
la función gaussiana y el tamaño del arreglo con el cual tu imagen es 
numéricamente convolucionada por FIND. Si las condiciones durante tu 
corrida fueron razonablemente constantes, un solo valor debería ser 
suficiente para todos tus marcos.
""",

"INFO_th": """
THRESHOLD: 
El nivel de significancia, en desviaciones estándar, que deseas que 
el programa utilice para decidir si un aumento positivo en el brillo 
es real. Normalmente, un valor alrededor de 4σ es bueno, pero puede 
que quieras establecerlo un poco más alto para la primera pasada.
""",

"INFO_ls": """
LOW SHARPNESS CUTOFF: 
El valor mínimo para la nitidez de un aumento de brillo que FIND 
considera una estrella real. Esto está destinado a eliminar píxeles 
defectuosos y también puede ayudar a eliminar galaxias de baja 
luminosidad. En la mayoría de los casos, los valores por defecto 
proporcionados en el programa son adecuados, pero si deseas afinar 
los valores, aquí están.
""",

"INFO_lr": """
HIGH SHARPNESS CUTOFF: 
El valor máximo para la nitidez de un aumento de brillo que FIND 
considera una estrella real. Esto está destinado a eliminar píxeles 
defectuosos y puede ayudar a eliminar algunas galaxias de baja 
luminosidad. Nuevamente, los valores predeterminados en el programa 
suelen ser adecuados para la mayoría de los casos, pero puedes 
ajustarlos según sea necesario.
""",

"INFO_hs": """
LOW ROUNDNESS CUTOFF: 
El valor mínimo para la redondez de un aumento de brillo que FIND 
considera una estrella real. Esto está destinado a eliminar filas y 
columnas defectuosas, y también puede rechazar algunas galaxias de 
borde. En general, los valores predeterminados en el programa son 
adecuados, pero pueden ser ajustados para casos especiales.
""",

"INFO_hr": """
HIGH ROUNDNESS CUTOFF: 
El valor máximo para la redondez de un aumento de brillo que FIND 
considera una estrella real. Esto está destinado a eliminar filas y 
columnas defectuosas, y puede rechazar algunas galaxias de borde. 
Los valores predeterminados en el programa suelen ser adecuados para 
la mayoría de los casos, pero puedes ajustarlos si es necesario.
""",

"INFO_wa": """
WATCH PROGRESS: 
Indica si se deben mostrar los resultados en el terminal de la 
computadora en tiempo real mientras se calculan. Mostrar los 
resultados puede mantenerte entretenido mientras se realizan las 
reducciones, pero puede ralentizar el tiempo de ejecución y, en modo 
por lotes, llenar excesivamente tu archivo de registro.
""",

"INFO_fi": """
FITTING RADIUS: 
Este parámetro define el área circular dentro de la cual se usarán los 
píxeles para realizar los ajustes de perfil en PEAK y NSTAR. La 
función de dispersión puntual se ajusta para determinar la posición 
y el brillo de cada estrella en tu programa. Solo los píxeles dentro 
de un radio de ajuste del centroide se usarán en el ajuste. También se 
usa para ajustar la primera aproximación analítica a la PSF para las 
estrellas PSF y contribuye en menor medida a la determinación de cuando 
las estrellas se superponen "significativamente". Bajo circunstancias 
normales, este radio debe ser del orden del FWHM de una imagen 
estelar. Cuando la aglomeración es extremadamente 
severa, puede ser ventajoso usar un valor algo menor que esto. Por otro 
lado, si se sabe que la PSF varía a través del marco, aumentar el radio 
de ajuste más allá del FWHM puede mejorar la precisión fotométrica, siempre 
que el campo no esté horrible.

Consejos:
    - Debería ser igual o un poco mayor que el FWHM para una fotometría 
      óptima.
""",

"INFO_ps": """
PSF RADIUS: 
El radio, en píxeles, del círculo dentro del cual se definirá la función 
de dispersión puntual (PSF). Debe ser algo mayor que el radio de la 
estrella más brillante de interés, como se mediría en tu visualización 
de imagen. Si al final de tus reducciones (ver §B arriba), notas que las 
imágenes substraídas de tus estrellas brillantes están rodeadas de halos 
luminosos con bordes internos agudos, entonces tu radio PSF es demasiado 
pequeño. Por otro lado, el tiempo de CPU requerido para las reducciones 
de ajuste de perfil es una función fuerte del radio PSF, por lo que es 
contraproducente hacer que este parámetro sea demasiado grande.

Consejos:
    - El valor de PSF debería ser 3 o 4 veces mayor que FWHM para 
      garantizar que las alas del PSF estén suficientemente alejadas.
""",

"INFO_va": """
VARIABLE PSF: 
El grado de complejidad con el que se modelará la PSF. En sus inicios, 
DAOPHOT Classic permitía solo una forma para el modelo PSF: una 
aproximación analítica gaussiana, más una tabla de corrección empírica 
del modelo analítico a la PSF "real". Esto corresponde a VARIABLE PSF = 0. 
Más tarde, se añadió la posibilidad de una PSF que varía linealmente con 
la posición en el marco; esto es VARIABLE PSF = 1. DAOPHOT II ahora permite 
dos posibilidades adicionales: una PSF que varía cuadráticamente con la 
posición en el marco (VARIABLE PSF = 2), y un modelo PSF puramente analítico, 
sin tabla de corrección empírica, como en ROMAFOT (VARIABLE PSF = -1). Es 
probable que sea mejor dejarlo en 0.0 (= "Constante") hasta que estés 
seguro de lo que estás haciendo.

Consejos:
    - Comienza siempre con VAR=0 (PSF constante) y revisa los resultados. 
      Modelos de PSF más complejos requieren más estrellas definidas.
""",

"INFO_an": """
ANALYTIC MODEL PSF: 
DAOPHOT Classic siempre usó una función gaussiana como una aproximación 
analítica a la PSF. DAOPHOT II permite una serie de alternativas que se 
discutirán a continuación bajo el comando PSF.

Consejos:
    - Para imágenes CCD normales, AN=1 (Gaussiana) suele ser adecuado.
    - Para imágenes HST/WFPC2, AN=4 (Lorentziana) es más adecuado.
    - Consulta el manual para más detalles sobre otras funciones.
""",

"INFO_ex": """
EXTRA PSF CLEANING PASSES: 
DAOPHOT II ahora puede reconocer y reducir el peso de píxeles claramente 
discordantes mientras genera una tabla de corrección PSF. Al igual que con 
FIND, puedes reducir el peso de píxeles discordantes mediante el comando 
PSF o durante el proceso de ajuste de perfil. El número de veces que el 
programa pasará por los datos es especificado por EXTRA PSF CLEANING PASSES. 
El valor típico es de 5 pasadas, aunque en la mayoría de los casos, 
establecer esto en 0 es suficiente.
""",

"INFO_us": """
FRACTIONAL PIXEL EXPANSION: 
Este parámetro no está implementado en DAOPHOT II. Déjalo tal como está. 
Es un remanente de DAOPHOT Classic.
""",

"INFO_pr": """
PROFILE ERROR: 
El error asociado a la PSF debido a la falta de precisión infinita en la 
función. La incertidumbre de los parámetros de perfil se ajusta al error 
específico. El error aumenta linealmente con la intensidad de la estrella 
y disminuye con la cuarta potencia del FWHM. Los valores por defecto 
proporcionados en el programa deberían ser adecuados, aunque en algunos 
casos puede ser necesario ajustar este parámetro para garantizar una 
precisión aceptable.
""",

"INFO_pe": """
PERCENT ERROR: 
Este parámetro especifica el valor de incertidumbre en la estructura fina 
del campo plano. Aumenta linealmente con la intensidad de la estrella más 
el fondo. La precisión del ajuste fotométrico y del modelo PSF está 
asociada a este parámetro. Los valores por defecto en el programa son 
adecuados en la mayoría de los casos, pero puede ser necesario ajustarlos 
para lograr una precisión más fina en algunos casos.
""",

"INFO_A1": """
RADIO DE APERTURA A1:
El radio de apertura en píxeles para la primera configuración específica. 
Este parámetro define el tamaño del área circular alrededor de cada estrella 
donde se mide el flujo de luz. La elección de un radio adecuado es crucial 
para la precisión de la fotometría. Un radio más grande captura más luz, 
pero también puede incluir más ruido, mientras que un radio más pequeño 
puede perder parte de la luz de la estrella. Ajusta el radio en función del 
tamaño de las estrellas y las condiciones de imagen para obtener medidas 
precisas y fiables.
""",

"INFO_A2": """
RADIO DE APERTURA A2:
El radio de apertura en píxeles para la segunda configuración específica. 
Este radio permite definir otro tamaño de apertura circular para medir el 
flujo de luz de las estrellas. Puedes utilizar radios consecutivos para 
comparar cómo cambia la medición del flujo con diferentes tamaños de apertura.
""",

"INFO_A3": """
RADIO DE APERTURA A3:
El radio de apertura en píxeles para la tercera configuración. Este radio 
define un área circular adicional para medir el flujo de luz de las estrellas. 
Consulta RADIO DE APERTURA A1 para más detalles sobre cómo elegir el radio 
adecuado.
""",

"INFO_A4": """
RADIO DE APERTURA A4:
El radio de apertura en píxeles para la cuarta configuración específica. 
Utiliza este radio para ajustar la medición del flujo estelar. Referencia 
RADIO DE APERTURA A1 para más información sobre el ajuste de los radios de apertura.
""",

"INFO_A5": """
RADIO DE APERTURA A5:
El radio de apertura en píxeles para la quinta configuración. Define otro 
tamaño de apertura circular para las mediciones estelares. Consulta RADIO 
DE APERTURA A1 para detalles sobre cómo estos radios afectan la precisión 
de las mediciones.
""",

"INFO_A6": """
RADIO DE APERTURA A6:
El radio de apertura en píxeles para la sexta configuración. Este radio 
especifica un área adicional para medir la luz de las estrellas. Refiérete 
a RADIO DE APERTURA A1 para ajustar los radios de apertura de acuerdo con 
tus necesidades.
""",

"INFO_A7": """
RADIO DE APERTURA A7:
El radio de apertura en píxeles para la séptima configuración. Utiliza 
este radio para definir un área circular para las mediciones estelares. 
Consulta RADIO DE APERTURA A1 para más detalles sobre la elección del radio 
adecuado.
""",

"INFO_A8": """
RADIO DE APERTURA A8:
El radio de apertura en píxeles para la octava configuración específica. 
Este radio define un área circular adicional para medir el flujo estelar. 
Refiérete a RADIO DE APERTURA A1 para información sobre cómo ajustar el radio 
para obtener medidas precisas.
""",

"INFO_A9": """
RADIO DE APERTURA A9:
El radio de apertura en píxeles para la novena configuración. Define un 
tamaño de apertura circular para medir la luz de las estrellas. Consulta 
RADIO DE APERTURA A1 para ajustar los radios de apertura y mejorar la 
precisión de las mediciones.
""",

"INFO_AA": """
RADIO DE APERTURA AA:
El radio de apertura en píxeles para la décima configuración específica. 
Utiliza este radio para definir un área circular adicional para las 
mediciones estelares. Referencia RADIO DE APERTURA A1 para detalles sobre 
cómo estos radios afectan tus mediciones.
""",

"INFO_AB": """
RADIO DE APERTURA AB:
El radio de apertura en píxeles para la undécima configuración. Define el 
tamaño de la apertura para medir el flujo de luz de las estrellas. Refiérete 
a RADIO DE APERTURA A1 para información adicional sobre la elección del radio.
""",

"INFO_AC": """
RADIO DE APERTURA AC:
El radio de apertura en píxeles para la duodécima configuración. Este radio 
especifica un área circular para la medición de la luz estelar. Consulta 
RADIO DE APERTURA A1 para ajustar el radio según las características de tus 
imágenes y estrellas.
""",

"INFO_a_is": """
INNER SKY RADIUS:
El RADIO INTERIOR DEL CIELO define el tamaño del área circular interna 
utilizada para calcular el valor del fondo del cielo alrededor de las 
estrellas. Este parámetro debe ser menor que el RADIO EXTERIOR DEL CIELO. 
Un valor pequeño (como 0.00) puede ser útil para minimizar la influencia del 
ruido cerca del centro de la estrella. Sin embargo, un valor muy pequeño puede 
llevar a una estimación menos precisa del fondo. Un valor típico podría ser 
igual al RADIO DE APERTURA o ligeramente mayor.

Consejos:
- Establece IS cerca del RADIO DE APERTURA o un poco mayor para una estimación 
    precisa del fondo del cielo.
- Un valor pequeño minimiza la influencia del ruido, pero asegúrate de que 
    sea suficientemente grande para captar una muestra representativa del fondo 
    del cielo.
""",

"INFO_a_os": """
OUTER SKY RADIUS:
El RADIO EXTERIOR DEL CIELO define el tamaño del área circular externa 
utilizada para calcular el valor del fondo del cielo. Este parámetro debe 
ser mayor que el RADIO INTERIOR DEL CIELO. Debe ser grande en comparación 
con el RADIO DE APERTURA para incluir suficientes datos del fondo del cielo y 
evitar influencias de variaciones locales. Un valor típico es comparable al 
RADIO DE APERTURA o un poco más grande.

Consejos:
- Establece OS significativamente mayor que IS para asegurar una buena 
    estimación del fondo del cielo.
- Un valor mayor que el RADIO DE APERTURA y comparable al RADIO DE APERTURA 
    suele ser adecuado para evitar influencias locales en el cálculo del fondo 
    del cielo.
""",

"INFO_sumaver": """
Number of frames averaged, summed:
Este parámetro indica el número de exposiciones independientes que se han 
promediado o sumado para crear el marco que estás a punto de reducir. La 
información proporcionada se utiliza para ajustar adecuadamente el ruido de 
lectura y la ganancia en el procesamiento de la imagen. El valor se introduce 
en un formato específico que indica cuántos marcos se han promediado y cuántos 
se han sumado. Por ejemplo:
- "5,1" significa que se promediaron cinco exposiciones y se sumaron.
- "1,5" indica que se sumaron cinco exposiciones y no se promediaron.
- "3,2" significa que se promediaron tres sumas de dos exposiciones cada una.
Si el marco representa la mediana de varias exposiciones independientes, 
se debe ingresar como si fuera el promedio de dos tercios del número de marcos. 
Por ejemplo, la mediana de tres imágenes se ingresaría como "2,1".

Consejos:
- Introduce el número de marcos promediados y sumados de acuerdo con el proceso 
    de reducción específico que seguiste.
- Usa el formato apropiado para reflejar con precisión el tratamiento de las 
    exposiciones en tus datos.
""",

"INFO_minmag": """
Minimum Magnitude:
Este parámetro define el límite inferior para la magnitud de las estrellas 
que serán incluidas en el análisis. Las estrellas con magnitudes inferiores 
a este valor serán consideradas para el ajuste y el cálculo. Establecer un 
valor adecuado para la magnitud mínima es crucial para asegurarse de que 
todas las estrellas relevantes se incluyan en el análisis sin introducir 
ruido innecesario.

Consejos:
- Ajusta el valor de la magnitud mínima para incluir todas las estrellas que 
    esperas analizar, excluyendo aquellas que podrían ser demasiado tenues o 
    no relevantes.
- Establece este parámetro con base en la sensibilidad de tu imagen y el rango 
    de magnitudes de las estrellas presentes en tu marco.
""",

"INFO_a_fi": """
FITTING RADIUS:
El RADIO DE AJUSTE determina el tamaño del área circular alrededor de 
cada estrella que se utiliza para ajustar la función de dispersión de 
puntos (PSF). Este parámetro es crucial para la precisión del ajuste, ya 
que define el rango dentro del cual se considera la información de la 
estrella para el ajuste. Un RADIO DE AJUSTE mayor incluye más datos en el 
ajuste, lo que puede mejorar la precisión pero también puede hacer que el 
ajuste sea menos sensible a variaciones locales en el fondo. Un RADIO DE 
AJUSTE menor puede ser más sensible a las variaciones locales y puede dar 
lugar a un ajuste más fino. En general, un valor típico para el RADIO DE 
AJUSTE es de 2.50 píxeles, pero puede necesitar ajustes dependiendo de la 
densidad de las estrellas y la calidad de la imagen.

Consejos:
- Para imágenes con estrellas no muy densas y en buena calidad, un valor 
    de 2.50 píxeles es un buen punto de partida.
- Ajusta el radio según la densidad estelar y la calidad de la imagen. Un 
    RADIO DE AJUSTE mayor puede ser necesario si las estrellas están muy 
    separadas, mientras que un valor menor puede ser más adecuado para 
    estrellas muy juntas.
""",

"INFO_a_re": """
REDETERMINE CENTROIDS:
Este parámetro indica si se deben recalcular las posiciones de los 
centroides de las estrellas durante el ajuste. Un valor de 1.00 significa 
que las posiciones de las estrellas se recalcularán para mejorar la 
precisión, mientras que un valor de 0.00 asume que las posiciones de las 
estrellas son conocidas con precisión y solo se ajustarán las magnitudes. 
Si se tiene confianza en las posiciones de las estrellas, se puede fijar 
a 0.00 para ahorrar tiempo de cálculo, pero si se necesita la máxima 
precisión, se debe usar 1.00.

Consejos:
- Usa 1.00 si necesitas ajustar tanto las posiciones como las magnitudes 
    para obtener la máxima precisión.
- Configura en 0.00 si las posiciones de las estrellas ya son precisas y 
    solo necesitas ajustar las magnitudes. Esto puede ahorrar tiempo de 
    procesamiento si se tiene una lista de estrellas precisa.
""",

"INFO_a_wa": """
WATCH PROGRESS:
El parámetro OBSERVAR PROGRESO controla la cantidad de información de 
progreso que se muestra durante la ejecución del programa en la terminal. 
Un valor de 1.00 activa la visualización de mensajes detallados sobre el 
progreso del procesamiento, lo que puede ser útil para monitorear la 
ejecución en tiempo real y detectar problemas. Un valor de 0.00 desactiva 
estos mensajes, lo que puede acelerar la ejecución si se está ejecutando 
en un entorno de producción o si no es necesario monitorear el progreso.

Consejos:
- Esta función no es muy útil en la interfaz, pero es bueno no olvidarla 
    si usas DAOPHOT.
- Si necesitas monitorear el progreso del procesamiento, usa 1.00.
- Para ejecuciones en segundo plano o en producción, especialmente si no 
    es necesario ver detalles del progreso, configura este valor en 0.00 
    para mejorar la eficiencia.
""",

"INFO_a_pe": """
PERCENT ERROR:
En el cálculo del error estándar esperado para el valor de brillo en un 
píxel, el programa utiliza el ruido de lectura y las estadísticas de 
Poisson del número esperado de fotones. Este parámetro permite especificar 
un valor particular para la incertidumbre de la estructura fina del campo 
plano. El error de Poisson aumenta con la raíz cuadrada de la intensidad, 
mientras que el ERROR PORCENTAJE aumenta linealmente con la intensidad del 
(estrella + cielo). Se refiere a la granularidad del desajuste del campo 
plano utilizado para calibrar las imágenes del programa.

Consejos:
- Deja este parámetro por defecto hasta que tengas una comprensión 
    avanzada del ajuste.
- Ajustar este parámetro puede afectar la precisión del ajuste en función 
    de la granularidad del campo plano.
""",

"INFO_a_ce": """
CLIPPING EXPONENT:
El EXPONENTE DE RECORTE es utilizado en la fórmula de recorte de datos para 
manejar datos atípicos. Este parámetro define el exponente en la fórmula 
de recorte y tiene un impacto significativo en la forma en que se manejan 
los datos atípicos. Un valor de 6.00 es el valor predeterminado, pero este 
parámetro puede ajustarse según la calidad y la distribución de los datos. 
Experimentar con diferentes valores puede ayudar a optimizar los resultados, 
aunque puede requerir mucho tiempo de CPU.

Consejos:
- Los valores predeterminados (6.00) suelen funcionar bien, pero puedes 
    ajustar el EXPONENTE DE RECORTE si encuentras que los datos atípicos 
    están afectando los resultados.
- Experimenta con diferentes valores si estás dispuesto a invertir tiempo 
    en la optimización, pero ten en cuenta que esto puede aumentar el tiempo 
    de procesamiento.
- Para una configuración más conservadora, establece el valor en 0.0 para 
    desactivar el recorte de datos atípicos.
""",

"INFO_a_cr": """
CLIPPING RANGE:
El RANGO DE RECORTE define el umbral en la fórmula de recorte que determina 
cuánto peso se le da a los datos atípicos. Un valor de 2.50 se usa por 
defecto, lo que significa que los residuos mayores a 2.5 veces la desviación 
estándar reciben la mitad del peso. Ajustar este parámetro puede ayudar a 
mejorar la robustez del ajuste frente a datos atípicos, pero también puede 
aumentar el tiempo de procesamiento. Valores de rango más altos pueden 
permitir una mayor influencia de los datos atípicos, mientras que valores 
más bajos pueden descartar más datos.

Consejos:
- Los valores predeterminados (2.50) suelen ser adecuados, pero puedes 
    ajustar el RANGO DE RECORTE si los datos atípicos están afectando la 
    precisión del ajuste.
- Ajusta este parámetro con cuidado, ya que valores más altos pueden 
    incluir más datos atípicos y valores más bajos pueden eliminar datos útiles.
""",

"INFO_a_ma": """
MAXIMUM GROUP SIZE:
El parámetro TAMAÑO MÁXIMO DEL GRUPO define el número máximo de estrellas 
permitidas en un solo grupo durante el procesamiento. Cuando el número de 
estrellas en un grupo excede este límite, ALLSTAR intentará dividir el grupo 
en subgrupos más pequeños. Aumentar el valor de TAMAÑO MÁXIMO DEL GRUPO puede 
reducir el número de estrellas descartadas debido a agrupaciones demasiado 
grandes, pero también puede aumentar el tiempo de procesamiento. Un valor 
típico podría ser 50, pero es importante ajustar este parámetro según la 
densidad de estrellas en las imágenes y la capacidad de procesamiento 
disponible.

Consejos:
- Para imágenes con alta densidad estelar, comienza con 50 y ajusta según 
    sea necesario.
- Valores más altos permiten retener más estrellas en el análisis, pero 
    también aumentan el tiempo de procesamiento. Si encuentras que muchas 
    estrellas están siendo descartadas, considera aumentar este valor.
""",

"INFO_a_pr": """
PROFILE ERROR:
Al ajustar la función de dispersión de puntos a las imágenes estelares 
reales, hay errores debido a que la función de dispersión no es conocida 
con precisión infinita. Este parámetro define la amplitud de esta 
contribución al modelo de ruido; el ERROR DE PERFIL aumenta linealmente 
con la intensidad de la estrella sola (sin cielo) e inversamente a la 
cuarta potencia del ancho completo a la mitad del máximo. Por lo tanto, 
este error crece en importancia relativa al ERROR PORCENTAJE a medida que 
mejora la visión.

Consejos:
- Deja este parámetro por defecto hasta que tengas una comprensión 
    avanzada del ajuste.
- Ajustar el ERROR DE PERFIL puede ser necesario si la calidad de la imagen 
    mejora significativamente.
""",

"INFO_a_is": """
INNER SKY RADIUS:
El RADIO INTERIOR DEL CIELO define el tamaño del área circular interna 
utilizada para calcular el valor del fondo del cielo alrededor de las 
estrellas. Este parámetro debe ser menor que el RADIO EXTERIOR DEL CIELO. 
Un valor pequeño (como 0.00) puede ser útil para minimizar la influencia del 
ruido cerca del centro de la estrella. Sin embargo, un valor muy pequeño puede 
llevar a una estimación menos precisa del fondo. Un valor típico podría ser 
igual al RADIO DE AJUSTE o ligeramente mayor.

Consejos:
- Establece IS cerca del RADIO DE AJUSTE o un poco mayor para una estimación 
    precisa del fondo del cielo.
- Un valor pequeño minimiza la influencia del ruido, pero asegúrate de que 
    sea suficientemente grande para captar una muestra representativa del fondo 
    del cielo.
""",

"INFO_a_os": """
OUTER SKY RADIUS:
El RADIO EXTERIOR DEL CIELO define el tamaño del área circular externa 
utilizada para calcular el valor del fondo del cielo. Este parámetro debe 
ser mayor que el RADIO INTERIOR DEL CIELO. Debe ser grande en comparación 
con el RADIO DE AJUSTE para incluir suficientes datos del fondo del cielo y 
evitar influencias de variaciones locales. Un valor típico es comparable al 
RADIO DE DISPERSIÓN DEL PSF o un poco más grande.

Consejos:
- Establece OS significativamente mayor que IS para asegurar una buena 
    estimación del fondo del cielo.
- Un valor mayor que el RADIO DE AJUSTE y comparable al RADIO DE DISPERSIÓN 
    DEL PSF suele ser adecuado para evitar influencias locales en el cálculo 
    del fondo del cielo.
"""

}
