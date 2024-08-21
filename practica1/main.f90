PROGRAM Main
    use cargarInventarios
    IMPLICIT NONE
    TYPE(Inventario), DIMENSION(:), ALLOCATABLE :: listInventario
    INTEGER :: opcion 
    CHARACTER(LEN=100) :: filename

    CALL menu()  

CONTAINS 

    RECURSIVE SUBROUTINE menu()
        PRINT *, '1. Cargar inventario'
        PRINT *, '2. Cargar movimientos'
        PRINT *, '3. Mostrar inventario'
        PRINT *, '4. Salir'
        READ(*,*) opcion

        SELECT CASE (opcion)
            CASE (1)
                PRINT *, 'Escriba la ruta'
                READ *, filename
                CALL leerInventario(filename,listInventario)
                CALL menu()  
            CASE (2)
                PRINT *,'escriba la ruta de  los movimientos'
                READ *, filename
                call leerCambios(filename,listInventario)
                CALL menu() 

            CASE (3)
                PRINT *, 'Se imprime lista'
                call imprimirInventario(listInventario)
                CALL menu()  

            CASE (4)
                PRINT *, 'Saliendo del programa.'
                

            CASE DEFAULT
                PRINT *, 'Opción inválida, por favor ingrese un número válido.'
                CALL menu()  

        END SELECT
    END SUBROUTINE menu

END PROGRAM main



