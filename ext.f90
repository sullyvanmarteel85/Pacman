module librairy
    implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! DECLARATION NOUVEAU TYPE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type :: remplissage
    integer :: state, Y, X
end type

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!2.création de la fonction de changement d'etat des remplissages
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

type(remplissage) function changement_etat_remplissage(rempi,rempf)
    implicit none

type(remplissage), intent(in) :: rempi
type(remplissage), intent(out) :: rempf

select case (rempi%state)
    case (2)
        rempf%state = 1
    case (3,4)
        rempf%state = 2
    case default
        rempf%state=rempi%state
end select

end function changement_etat_remplissage

end module
