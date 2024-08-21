

MODULE cargarInventarios
    TYPE :: Inventario
        CHARACTER(LEN=50) :: nombre
        INTEGER :: cantidad
        REAL :: precio
        CHARACTER(LEN=50) :: ubicacion
    END TYPE Inventario
  
    CONTAINS
  
    SUBROUTINE leerInventario(filename, listInventario)
        TYPE(Inventario), DIMENSION(:), ALLOCATABLE :: listInventario
        CHARACTER(LEN=*), INTENT(IN) :: filename
        INTEGER :: i, ios, count, pos
        CHARACTER(LEN=50) :: line
        CHARACTER(LEN=50) :: dataTemp(4)
        CHARACTER(LEN=50) :: comando
    
        ! Contar la cantidad de líneas (registros) en el archivo
        count = 0
        OPEN(UNIT=10, FILE=filename, STATUS='OLD', ACTION='READ', iostat=ios)
        IF (ios /= 0) THEN
            PRINT *, 'Error al abrir el archivo'
            STOP
        END IF
    
        ! Contar el número de líneas en el archivo
        DO
            READ(10, '(A)', IOSTAT=ios) line
            IF (ios /= 0) EXIT
            count = count + 1
        END DO
    
        ! Volver a la primera línea del archivo
        REWIND(10)
        ALLOCATE(listInventario(count))
    
        ! Leer cada línea y separar los campos
        DO i = 1, count
            READ(10, '(A)') line
            
            ! Extraer y eliminar el comando (crear_equipo) de la línea
            pos = INDEX(line, ' ')
            comando = line(1:pos-1)
            line = line(pos+1:)
    
            CALL separarDatos(line,4,dataTemp)
    
            ! Asignar los valores a los campos del tipo Inventario
            listInventario(i)%nombre = TRIM(dataTemp(1))
            READ(dataTemp(2), *) listInventario(i)%cantidad
            READ(dataTemp(3), *) listInventario(i)%precio
            listInventario(i)%ubicacion = TRIM(dataTemp(4))
        END DO
        print*, 'se leyo el inventario de entrada'
        ! Cerrar el archivo
        CLOSE(10)
    END SUBROUTINE leerInventario
    SUBROUTINE separarDatos(line, numCampos, dataTemp)
        CHARACTER(LEN=*), INTENT(IN) :: line
        INTEGER, INTENT(IN) :: numCampos
        CHARACTER(LEN=50), DIMENSION(:), INTENT(OUT) :: dataTemp
        CHARACTER(LEN=256) :: local_line
        INTEGER :: pos, j
    
        ! Hacer una copia local de la línea para poder modificarla
        local_line = line
    
        ! Inicializar los campos en vacío
        DO j = 1, numCampos
            dataTemp(j) = ''
        END DO
    
        ! Separar los campos por los ';'
        DO j = 1, numCampos
            pos = INDEX(local_line, ';')
            IF (pos > 0) THEN
                dataTemp(j) = TRIM(local_line(1:pos-1))
                local_line = local_line(pos+1:)
            ELSE
                dataTemp(j) = TRIM(local_line)
                EXIT
            END IF
        END DO
    END SUBROUTINE separarDatos
    SUBROUTINE imprimirInventario(listInventario)
        TYPE(Inventario), DIMENSION(:), INTENT(IN) :: listInventario
        INTEGER :: i, ios
        REAL :: valorTotal
        CHARACTER(LEN=100) :: nombreArchivo
    
        ! Nombre del archivo de salida
        nombreArchivo = 'informeInventario.txt'
    
        ! Abrir el archivo para escribir
        OPEN(UNIT=10, FILE=nombreArchivo, STATUS='REPLACE', ACTION='WRITE', IOSTAT=ios)
        IF (ios /= 0) THEN
            PRINT *, 'Error al abrir el archivo:', TRIM(nombreArchivo)
            RETURN
        END IF
    
        ! Escribir el encabezado del informe
        WRITE(10, '(A)') 'Informe de Inventario:'
        WRITE(10, '(A)') '------------------------------------------------------------'
        WRITE(10, '(A, A, A, A, A)') 'Equipo', '    Cantidad', '    Precio Unitario', '    Valor Total', '    Ubicación'
        WRITE(10, '(A)') '------------------------------------------------------------'
    
        ! Escribir cada registro del inventario en formato de tabla
        DO i = 1, SIZE(listInventario)
            valorTotal = listInventario(i)%cantidad * listInventario(i)%precio
            WRITE(10, '(A20,5X, I6,5X,  F10.2, 5X, F10.2,5X,  A20)') TRIM(listInventario(i)%nombre), &
                  listInventario(i)%cantidad, listInventario(i)%precio, valorTotal, TRIM(listInventario(i)%ubicacion)
        END DO
    
        ! Cerrar el archivo
        CLOSE(10)
    
        PRINT *, 'Informe de inventario generado en:', TRIM(nombreArchivo)
    END SUBROUTINE imprimirInventario

    SUBROUTINE leerCambios(filename, listInventario)
        CHARACTER(LEN=*), INTENT(IN) :: filename
        TYPE(Inventario), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: listInventario
        CHARACTER(LEN=50) :: line, comando, nombre, ubicacion
        INTEGER :: cantidad, ios, pos
        
        CHARACTER(LEN=50) :: dataTemp(4)

        OPEN(UNIT=20, FILE=filename, STATUS='OLD', ACTION='READ', IOSTAT=ios)
        IF (ios /= 0) THEN
            PRINT *, 'Error al abrir el archivo de cambios'
            STOP
        END IF
    
        DO
            READ(20, '(A)', IOSTAT=ios) line
            IF (ios /= 0) EXIT
    
            ! Extraer el comando (agregar_stock o eliminar_equipo)
            pos = INDEX(line, ' ')
            comando = line(1:pos-1)
            line = line(pos+1:)
    
            ! Separar el resto de la línea en sus componentes
            CALL separarDatos(line,3,dataTemp)

            nombre = TRIM(dataTemp(1))
            READ(dataTemp(2), *) cantidad
            ubicacion = TRIM(dataTemp(3))
            
        


    
            ! Llamar a la subrutina correspondiente
            IF (TRIM(comando) == 'agregar_stock') THEN
                CALL agregarStock(listInventario, nombre, cantidad, ubicacion)
            ELSE IF (TRIM(comando) == 'eliminar_equipo') THEN
                CALL eliminarEquipo(listInventario, nombre, cantidad, ubicacion)
            ELSE
                PRINT *, 'Comando no reconocido:', TRIM(comando)
            END IF
        END DO
    
        CLOSE(20)
    END SUBROUTINE leerCambios
    
    SUBROUTINE agregarStock(listInventario, nombre, cantidad, ubicacion)
    TYPE(Inventario), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: listInventario
    CHARACTER(LEN=50), INTENT(IN) :: nombre, ubicacion
    INTEGER, INTENT(IN) :: cantidad
    INTEGER :: i
    LOGICAL :: encontrado

    encontrado = .FALSE.
    DO i = 1, SIZE(listInventario)
        IF (TRIM(listInventario(i)%nombre) == TRIM(nombre) .AND. &
            TRIM(listInventario(i)%ubicacion) == TRIM(ubicacion)) THEN
            listInventario(i)%cantidad = listInventario(i)%cantidad + cantidad
            encontrado = .TRUE.
            PRINT *, 'Stock actualizado para:', TRIM(nombre), 'en', TRIM(ubicacion)
            EXIT
        END IF
    END DO

    IF (.NOT. encontrado) THEN
        PRINT *, 'Equipo no encontrado para agregar:', TRIM(nombre), TRIM(ubicacion)
    END IF
END SUBROUTINE agregarStock

SUBROUTINE eliminarEquipo(listInventario, nombre, cantidad, ubicacion)
    TYPE(Inventario), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: listInventario
    CHARACTER(LEN=50), INTENT(IN) :: nombre, ubicacion
    INTEGER, INTENT(IN) :: cantidad
    INTEGER :: i
    LOGICAL :: encontrado

    encontrado = .FALSE.
    DO i = 1, SIZE(listInventario)
        IF (TRIM(listInventario(i)%nombre) == TRIM(nombre) .AND. &
            TRIM(listInventario(i)%ubicacion) == TRIM(ubicacion)) THEN
            encontrado = .TRUE.

            ! Comprobar si la cantidad a eliminar es mayor que la existente
            IF (cantidad > listInventario(i)%cantidad) THEN
                PRINT *, 'Error: La cantidad a eliminar (', cantidad, ') es mayor que la existente (', &
                          listInventario(i)%cantidad, ') para:', TRIM(nombre), 'en', TRIM(ubicacion)
            ELSE
                ! Actualizar la cantidad o eliminar el equipo si la cantidad es 0 o menor
                listInventario(i)%cantidad = listInventario(i)%cantidad - cantidad
                IF (listInventario(i)%cantidad <= 0) THEN
                    listInventario(i)%cantidad = 0
                    PRINT *, 'Equipo eliminado:', TRIM(listInventario(i)%nombre)
                ELSE
                    PRINT *, 'Stock actualizado para:', TRIM(nombre), 'en', TRIM(ubicacion), &
                             'Nueva cantidad:', listInventario(i)%cantidad
                END IF
            END IF

            EXIT
        END IF
    END DO

    IF (.NOT. encontrado) THEN
        PRINT *, 'Error: Equipo no encontrado para eliminar:', TRIM(nombre), TRIM(ubicacion)
    END IF
END SUBROUTINE eliminarEquipo
END MODULE cargarInventarios 