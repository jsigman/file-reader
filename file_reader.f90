module file_reader
    contains
   
    function read_data_file(fileName) result(A)
        implicit none
        !inputs
        character*(*) fileName
        !output
        real*8, dimension(:,:), allocatable :: A
        !local variables
        integer,parameter :: MAX_FILE_ROWS = 10000 !constant
        integer,parameter :: MAX_FILE_COLUMNS = 10000 !constant
        integer,parameter :: MAX_ASCII_NUMBER_SIZE = 25
        integer,parameter :: UNDEF = -9797
        real*8, dimension(:),allocatable :: dummyLineArray !read all the lines into an array
        character(LEN = MAX_FILE_COLUMNS*MAX_ASCII_NUMBER_SIZE) dummyLine
        integer M,N !Rows and columns in the output matrix, we'll need to read these from the file first
        integer i,j

        !start of procedure
        print *,'Reading file: ', fileName
        open(unit=9,file=fileName,status='old') 
        M = 0
        do i = 1,MAX_FILE_ROWS
            !step through each line, discarding each, until we get to the end of the file
            read(9,*,end = 1)
            M = M+1
        enddo
        !M now has the number of rows in the file
        1 REWIND(9)
        !start back at the beginning of the file
        read(9,'(a)') dummyLine !read the first line in as a single string
        allocate(dummyLineArray(MAX_FILE_COLUMNS))
        dummyLineArray = UNDEF !initialize
        read(dummyLine,*,end=2) (dummyLineArray(i),i=1,MAX_FILE_COLUMNS) !parse the first line
        2 N = COUNT(dummyLineArray /= UNDEF) !non-empty elements give us the number of column elements
        deallocate(dummyLineArray) !this is a big array, free this memory

        !one last run through the file, this time to extract the elements
        REWIND(9)
        allocate(A(M,N))
        A = 0
        do i = 1,M
            read(9,*)(A(i,j),j=1,N)
        enddo
        close(9)
        RETURN
    end function read_data_file
    
    subroutine write_data_file(fileName,A)
        implicit none
        !inputs
        character*(*) fileName
        !output
        real*8, dimension(:,:) :: A
        !local variables
        integer M,N !Rows and columns in the  matrix
        integer i,j

        !start of procedure
        print *,'Writing file: ', fileName
        M = SIZE(A,1)
        N = SIZE(A,2)

        open(unit = 9,file = fileName,status = 'UNKNOWN')
        do i = 1, M
            write(9,*) (A(i,j),j=1,N)
        enddo
        close(9)
        
        print *,'File ''',trim(fileName),''' successfully written. ',M,' rows, ',N,' columns'
        return
    end subroutine write_data_file
end module file_reader
