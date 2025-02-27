































c











































































































































































 



































































































































































 





















































 










 








































































































































































































































 










                                
                                

                                
                                

                                
                                

                                






























































































































































































c
c

c
c
c
c
c
c

c
c
c
c
c

c

c
c
c
c
c
c
c
c
c
c
c
c

c







c

c

c
c

c




c

c
	subroutine Exact__decode_pars(cctk_dim,cctk_gsh,cctk_lsh,cctk_lbnd,cctk
     &_ubnd,cctk_level,cctk_patch,cctk_npatches,cctk_component,cctk_tile
     &_min,cctk_tile_max,cctk_ash,cctk_alignment,cctk_alignment_offset,c
     &ctk_from,cctk_to,cctk_bbox,cctk_delta_time,cctk_time,cctk_delta_sp
     &ace,cctk_origin_space,cctk_levfac,cctk_levoff,cctk_levoffdenom,cct
     &k_timefac,cctk_convlevel,cctk_convfac,cctk_nghostzones,cctk_iterat
     &ion,cctkGH,cctk_ash1,cctk_ash2,cctk_ash3, slicet,slicetmp1t,slicet
     &mp1x,slicetmp1y,slicetmp1z,slicetmp2t,slicetmp2x,slicetmp2y,slicet
     &mp2z,slicex,slicey,slicez, Bertotti___Lambda,Kasner_generalized___
     &p1,Kasner_generalized___p2,Kasner_like___q,Lemaitre___Lambda,Lemai
     &tre___R0,Lemaitre___epsilon0,Lemaitre___kappa,Schwarzschild_Lemait
     &re___Lambda,Schwarzschild_Lemaitre___mass,anti_de_Sitter_Lambda___
     &scale,constant_density_star___mass,constant_density_star___radius,
     &de_Sitter_Lambda___scale,de_Sitter___scale,decoded_exact_model, ac
     &tive_slicing_handle,alp,alp_p,alp_p_p,betax,betax_p,betax_p_p,beta
     &y,betay_p,betay_p_p,betaz,betaz_p,betaz_p_p,coarse_dx,coarse_dy,co
     &arse_dz,conformal_state,dtalp,dtalp_p,dtalp_p_p,dtbetax,dtbetax_p,
     &dtbetax_p_p,dtbetay,dtbetay_p,dtbetay_p_p,dtbetaz,dtbetaz_p,dtbeta
     &z_p_p,dtlapse_state,dtshift_state,gxx,gxx_p,gxx_p_p,gxy,gxy_p,gxy_
     &p_p,gxz,gxz_p,gxz_p_p,gyy,gyy_p,gyy_p_p,gyz,gyz_p,gyz_p_p,gzz,gzz_
     &p,gzz_p_p,kxx,kxx_p,kxx_p_p,kxy,kxy_p,kxy_p_p,kxz,kxz_p,kxz_p_p,ky
     &y,kyy_p,kyy_p_p,kyz,kyz_p,kyz_p_p,kzz,kzz_p,kzz_p_p,psi,psix,psixx
     &,psixy,psixz,psiy,psiyy,psiyz,psiz,psizz,r,shift_state,x,y,z)
	implicit none

      INTEGER cctk_dim
      integer, parameter :: cctki_use_cctk_dim = kind(cctk_dim)
      INTEGER cctk_gsh (cctk_dim)
      integer, parameter :: cctki_use_cctk_gsh = kind(cctk_gsh)
      INTEGER cctk_lsh (cctk_dim)
      integer, parameter :: cctki_use_cctk_lsh = kind(cctk_lsh)
      INTEGER cctk_lbnd (cctk_dim)
      integer, parameter :: cctki_use_cctk_lbnd = kind(cctk_lbnd)
      INTEGER cctk_ubnd (cctk_dim)
      integer, parameter :: cctki_use_cctk_ubnd = kind(cctk_ubnd)
      INTEGER cctk_level
      integer, parameter :: cctki_use_cctk_level = kind(cctk_level)
      INTEGER cctk_patch
      integer, parameter :: cctki_use_cctk_patch = kind(cctk_patch)
      INTEGER cctk_npatches
      integer, parameter :: cctki_use_cctk_npatches = kind(cctk_npatches
     &)
      INTEGER cctk_component
      integer, parameter :: cctki_use_cctk_component = kind(cctk_compone
     &nt)
      INTEGER cctk_tile_min (cctk_dim)
      integer, parameter :: cctki_use_cctk_tile_min = kind(cctk_tile_min
     &)
      INTEGER cctk_tile_max (cctk_dim)
      integer, parameter :: cctki_use_cctk_tile_max = kind(cctk_tile_max
     &)
      INTEGER cctk_ash (cctk_dim)
      integer, parameter :: cctki_use_cctk_ash = kind(cctk_ash)
      INTEGER cctk_alignment
      integer, parameter :: cctki_use_cctk_alignment = kind(cctk_alignme
     &nt)
      INTEGER cctk_alignment_offset
      integer, parameter :: cctki_use_cctk_alignment_offset = kind(cctk_
     &alignment_offset)
      INTEGER cctk_from (cctk_dim)
      integer, parameter :: cctki_use_cctk_from = kind(cctk_from)
      INTEGER cctk_to (cctk_dim)
      integer, parameter :: cctki_use_cctk_to = kind(cctk_to)
      INTEGER cctk_bbox (2*cctk_dim)
      integer, parameter :: cctki_use_cctk_bbox = kind(cctk_bbox)
      REAL*8 cctk_delta_time
      integer, parameter :: cctki_use_cctk_delta_time = kind(cctk_delta_
     &time)
      REAL*8 cctk_time
      integer, parameter :: cctki_use_cctk_time = kind(cctk_time)
      REAL*8 cctk_delta_space (cctk_dim)
      integer, parameter :: cctki_use_cctk_delta_space = kind(cctk_delta
     &_space)
      REAL*8 cctk_origin_space (cctk_dim)
      integer, parameter :: cctki_use_cctk_origin_space = kind(cctk_orig
     &in_space)
      INTEGER cctk_levfac (cctk_dim)
      integer, parameter :: cctki_use_cctk_levfac = kind(cctk_levfac)
      INTEGER cctk_levoff (cctk_dim)
      integer, parameter :: cctki_use_cctk_levoff = kind(cctk_levoff)
      INTEGER cctk_levoffdenom (cctk_dim)
      integer, parameter :: cctki_use_cctk_levoffdenom = kind(cctk_levof
     &fdenom)
      INTEGER cctk_timefac
      integer, parameter :: cctki_use_cctk_timefac = kind(cctk_timefac)
      INTEGER cctk_convlevel
      integer, parameter :: cctki_use_cctk_convlevel = kind(cctk_convlev
     &el)
      INTEGER cctk_convfac
      integer, parameter :: cctki_use_cctk_convfac = kind(cctk_convfac)
      INTEGER cctk_nghostzones (cctk_dim)
      integer, parameter :: cctki_use_cctk_nghostzones = kind(cctk_nghos
     &tzones)
      INTEGER cctk_iteration
      integer, parameter :: cctki_use_cctk_iteration = kind(cctk_iterati
     &on)
      integer*8 cctkGH
      integer, parameter :: cctki_use_cctkGH = kind(cctkGH)
      INTEGER cctk_ash1
      integer, parameter :: cctki_use_cctk_ash1 = kind(cctk_ash1)
      INTEGER cctk_ash2
      integer, parameter :: cctki_use_cctk_ash2 = kind(cctk_ash2)
      INTEGER cctk_ash3
      integer, parameter :: cctki_use_cctk_ash3 = kind(cctk_ash3)
      type cctki_inaccessible_grid_variable
      integer :: dummy
      end type
      INTEGER*4 :: decoded_exact_model
      integer, parameter :: cctki_use_decoded_exact_model = kind(decoded
     &_exact_model)
      REAL*8 :: anti_de_sitter_lambda___scale
      integer, parameter :: cctki_use_anti_de_sitter_lambda___scale = ki
     &nd(anti_de_sitter_lambda___scale)
      REAL*8 :: bertotti___lambda
      integer, parameter :: cctki_use_bertotti___lambda = kind(bertotti_
     &__lambda)
      REAL*8 :: constant_density_star___mass
      integer, parameter :: cctki_use_constant_density_star___mass = kin
     &d(constant_density_star___mass)
      REAL*8 :: constant_density_star___radius
      integer, parameter :: cctki_use_constant_density_star___radius = k
     &ind(constant_density_star___radius)
      REAL*8 :: de_sitter___scale
      integer, parameter :: cctki_use_de_sitter___scale = kind(de_sitter
     &___scale)
      REAL*8 :: de_sitter_lambda___scale
      integer, parameter :: cctki_use_de_sitter_lambda___scale = kind(de
     &_sitter_lambda___scale)
      REAL*8 :: kasner_generalized___p1
      integer, parameter :: cctki_use_kasner_generalized___p1 = kind(kas
     &ner_generalized___p1)
      REAL*8 :: kasner_generalized___p2
      integer, parameter :: cctki_use_kasner_generalized___p2 = kind(kas
     &ner_generalized___p2)
      REAL*8 :: kasner_like___q
      integer, parameter :: cctki_use_kasner_like___q = kind(kasner_like
     &___q)
      REAL*8 :: lemaitre___epsilon0
      integer, parameter :: cctki_use_lemaitre___epsilon0 = kind(lemaitr
     &e___epsilon0)
      REAL*8 :: lemaitre___kappa
      integer, parameter :: cctki_use_lemaitre___kappa = kind(lemaitre__
     &_kappa)
      REAL*8 :: lemaitre___lambda
      integer, parameter :: cctki_use_lemaitre___lambda = kind(lemaitre_
     &__lambda)
      REAL*8 :: lemaitre___r0
      integer, parameter :: cctki_use_lemaitre___r0 = kind(lemaitre___r0
     &)
      REAL*8 :: schwarzschild_lemaitre___lambda
      integer, parameter :: cctki_use_schwarzschild_lemaitre___lambda = 
     &kind(schwarzschild_lemaitre___lambda)
      REAL*8 :: schwarzschild_lemaitre___mass
      integer, parameter :: cctki_use_schwarzschild_lemaitre___mass = ki
     &nd(schwarzschild_lemaitre___mass)
      type(cctki_inaccessible_grid_variable), intent(IN) :: slicet
      integer, parameter :: cctki_use_slicet = kind(slicet%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: slicetmp1t
      integer, parameter :: cctki_use_slicetmp1t = kind(slicetmp1t%dummy
     &)
      type(cctki_inaccessible_grid_variable), intent(IN) :: slicetmp1x
      integer, parameter :: cctki_use_slicetmp1x = kind(slicetmp1x%dummy
     &)
      type(cctki_inaccessible_grid_variable), intent(IN) :: slicetmp1y
      integer, parameter :: cctki_use_slicetmp1y = kind(slicetmp1y%dummy
     &)
      type(cctki_inaccessible_grid_variable), intent(IN) :: slicetmp1z
      integer, parameter :: cctki_use_slicetmp1z = kind(slicetmp1z%dummy
     &)
      type(cctki_inaccessible_grid_variable), intent(IN) :: slicetmp2t
      integer, parameter :: cctki_use_slicetmp2t = kind(slicetmp2t%dummy
     &)
      type(cctki_inaccessible_grid_variable), intent(IN) :: slicetmp2x
      integer, parameter :: cctki_use_slicetmp2x = kind(slicetmp2x%dummy
     &)
      type(cctki_inaccessible_grid_variable), intent(IN) :: slicetmp2y
      integer, parameter :: cctki_use_slicetmp2y = kind(slicetmp2y%dummy
     &)
      type(cctki_inaccessible_grid_variable), intent(IN) :: slicetmp2z
      integer, parameter :: cctki_use_slicetmp2z = kind(slicetmp2z%dummy
     &)
      type(cctki_inaccessible_grid_variable), intent(IN) :: slicex
      integer, parameter :: cctki_use_slicex = kind(slicex%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: slicey
      integer, parameter :: cctki_use_slicey = kind(slicey%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: slicez
      integer, parameter :: cctki_use_slicez = kind(slicez%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: active_slici
     &ng_handle
      integer, parameter :: cctki_use_active_slicing_handle = kind(activ
     &e_slicing_handle%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: alp
      integer, parameter :: cctki_use_alp = kind(alp%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: alp_p
      integer, parameter :: cctki_use_alp_p = kind(alp_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: alp_p_p
      integer, parameter :: cctki_use_alp_p_p = kind(alp_p_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: betax
      integer, parameter :: cctki_use_betax = kind(betax%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: betax_p
      integer, parameter :: cctki_use_betax_p = kind(betax_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: betax_p_p
      integer, parameter :: cctki_use_betax_p_p = kind(betax_p_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: betay
      integer, parameter :: cctki_use_betay = kind(betay%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: betay_p
      integer, parameter :: cctki_use_betay_p = kind(betay_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: betay_p_p
      integer, parameter :: cctki_use_betay_p_p = kind(betay_p_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: betaz
      integer, parameter :: cctki_use_betaz = kind(betaz%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: betaz_p
      integer, parameter :: cctki_use_betaz_p = kind(betaz_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: betaz_p_p
      integer, parameter :: cctki_use_betaz_p_p = kind(betaz_p_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: coarse_dx
      integer, parameter :: cctki_use_coarse_dx = kind(coarse_dx%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: coarse_dy
      integer, parameter :: cctki_use_coarse_dy = kind(coarse_dy%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: coarse_dz
      integer, parameter :: cctki_use_coarse_dz = kind(coarse_dz%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: conformal_st
     &ate
      integer, parameter :: cctki_use_conformal_state = kind(conformal_s
     &tate%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: dtalp
      integer, parameter :: cctki_use_dtalp = kind(dtalp%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: dtalp_p
      integer, parameter :: cctki_use_dtalp_p = kind(dtalp_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: dtalp_p_p
      integer, parameter :: cctki_use_dtalp_p_p = kind(dtalp_p_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: dtbetax
      integer, parameter :: cctki_use_dtbetax = kind(dtbetax%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: dtbetax_p
      integer, parameter :: cctki_use_dtbetax_p = kind(dtbetax_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: dtbetax_p_p
      integer, parameter :: cctki_use_dtbetax_p_p = kind(dtbetax_p_p%dum
     &my)
      type(cctki_inaccessible_grid_variable), intent(IN) :: dtbetay
      integer, parameter :: cctki_use_dtbetay = kind(dtbetay%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: dtbetay_p
      integer, parameter :: cctki_use_dtbetay_p = kind(dtbetay_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: dtbetay_p_p
      integer, parameter :: cctki_use_dtbetay_p_p = kind(dtbetay_p_p%dum
     &my)
      type(cctki_inaccessible_grid_variable), intent(IN) :: dtbetaz
      integer, parameter :: cctki_use_dtbetaz = kind(dtbetaz%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: dtbetaz_p
      integer, parameter :: cctki_use_dtbetaz_p = kind(dtbetaz_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: dtbetaz_p_p
      integer, parameter :: cctki_use_dtbetaz_p_p = kind(dtbetaz_p_p%dum
     &my)
      type(cctki_inaccessible_grid_variable), intent(IN) :: dtlapse_stat
     &e
      integer, parameter :: cctki_use_dtlapse_state = kind(dtlapse_state
     &%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: dtshift_stat
     &e
      integer, parameter :: cctki_use_dtshift_state = kind(dtshift_state
     &%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: gxx
      integer, parameter :: cctki_use_gxx = kind(gxx%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: gxx_p
      integer, parameter :: cctki_use_gxx_p = kind(gxx_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: gxx_p_p
      integer, parameter :: cctki_use_gxx_p_p = kind(gxx_p_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: gxy
      integer, parameter :: cctki_use_gxy = kind(gxy%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: gxy_p
      integer, parameter :: cctki_use_gxy_p = kind(gxy_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: gxy_p_p
      integer, parameter :: cctki_use_gxy_p_p = kind(gxy_p_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: gxz
      integer, parameter :: cctki_use_gxz = kind(gxz%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: gxz_p
      integer, parameter :: cctki_use_gxz_p = kind(gxz_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: gxz_p_p
      integer, parameter :: cctki_use_gxz_p_p = kind(gxz_p_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: gyy
      integer, parameter :: cctki_use_gyy = kind(gyy%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: gyy_p
      integer, parameter :: cctki_use_gyy_p = kind(gyy_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: gyy_p_p
      integer, parameter :: cctki_use_gyy_p_p = kind(gyy_p_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: gyz
      integer, parameter :: cctki_use_gyz = kind(gyz%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: gyz_p
      integer, parameter :: cctki_use_gyz_p = kind(gyz_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: gyz_p_p
      integer, parameter :: cctki_use_gyz_p_p = kind(gyz_p_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: gzz
      integer, parameter :: cctki_use_gzz = kind(gzz%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: gzz_p
      integer, parameter :: cctki_use_gzz_p = kind(gzz_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: gzz_p_p
      integer, parameter :: cctki_use_gzz_p_p = kind(gzz_p_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: kxx
      integer, parameter :: cctki_use_kxx = kind(kxx%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: kxx_p
      integer, parameter :: cctki_use_kxx_p = kind(kxx_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: kxx_p_p
      integer, parameter :: cctki_use_kxx_p_p = kind(kxx_p_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: kxy
      integer, parameter :: cctki_use_kxy = kind(kxy%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: kxy_p
      integer, parameter :: cctki_use_kxy_p = kind(kxy_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: kxy_p_p
      integer, parameter :: cctki_use_kxy_p_p = kind(kxy_p_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: kxz
      integer, parameter :: cctki_use_kxz = kind(kxz%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: kxz_p
      integer, parameter :: cctki_use_kxz_p = kind(kxz_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: kxz_p_p
      integer, parameter :: cctki_use_kxz_p_p = kind(kxz_p_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: kyy
      integer, parameter :: cctki_use_kyy = kind(kyy%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: kyy_p
      integer, parameter :: cctki_use_kyy_p = kind(kyy_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: kyy_p_p
      integer, parameter :: cctki_use_kyy_p_p = kind(kyy_p_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: kyz
      integer, parameter :: cctki_use_kyz = kind(kyz%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: kyz_p
      integer, parameter :: cctki_use_kyz_p = kind(kyz_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: kyz_p_p
      integer, parameter :: cctki_use_kyz_p_p = kind(kyz_p_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: kzz
      integer, parameter :: cctki_use_kzz = kind(kzz%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: kzz_p
      integer, parameter :: cctki_use_kzz_p = kind(kzz_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: kzz_p_p
      integer, parameter :: cctki_use_kzz_p_p = kind(kzz_p_p%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: psi
      integer, parameter :: cctki_use_psi = kind(psi%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: psix
      integer, parameter :: cctki_use_psix = kind(psix%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: psixx
      integer, parameter :: cctki_use_psixx = kind(psixx%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: psixy
      integer, parameter :: cctki_use_psixy = kind(psixy%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: psixz
      integer, parameter :: cctki_use_psixz = kind(psixz%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: psiy
      integer, parameter :: cctki_use_psiy = kind(psiy%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: psiyy
      integer, parameter :: cctki_use_psiyy = kind(psiyy%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: psiyz
      integer, parameter :: cctki_use_psiyz = kind(psiyz%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: psiz
      integer, parameter :: cctki_use_psiz = kind(psiz%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: psizz
      integer, parameter :: cctki_use_psizz = kind(psizz%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: r
      integer, parameter :: cctki_use_r = kind(r%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: shift_state
      integer, parameter :: cctki_use_shift_state = kind(shift_state%dum
     &my)
      type(cctki_inaccessible_grid_variable), intent(IN) :: x
      integer, parameter :: cctki_use_x = kind(x%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: y
      integer, parameter :: cctki_use_y = kind(y%dummy)
      type(cctki_inaccessible_grid_variable), intent(IN) :: z
      integer, parameter :: cctki_use_z = kind(z%dummy)
	REAL*8 Alvi__mass1
      integer, parameter :: cctki_use_Alvi__mass1 = kind(Alvi__mass1)
      REAL*8 Alvi__mass2
      integer, parameter :: cctki_use_Alvi__mass2 = kind(Alvi__mass2)
      REAL*8 Alvi__separation
      integer, parameter :: cctki_use_Alvi__separation = kind(Alvi__sepa
     &ration)
      REAL*8 Bertotti__Lambda
      integer, parameter :: cctki_use_Bertotti__Lambda = kind(Bertotti__
     &Lambda)
      REAL*8 Bianchi_I__scale
      integer, parameter :: cctki_use_Bianchi_I__scale = kind(Bianchi_I_
     &_scale)
      REAL*8 Goedel__scale
      integer, parameter :: cctki_use_Goedel__scale = kind(Goedel__scale
     &)
      REAL*8 Gowdy_wave__amplitude
      integer, parameter :: cctki_use_Gowdy_wave__amplitude = kind(Gowdy
     &_wave__amplitude)
      REAL*8 Kasner_generalized__p1
      integer, parameter :: cctki_use_Kasner_generalized__p1 = kind(Kasn
     &er_generalized__p1)
      REAL*8 Kasner_generalized__p2
      integer, parameter :: cctki_use_Kasner_generalized__p2 = kind(Kasn
     &er_generalized__p2)
      REAL*8 Kasner_like__q
      integer, parameter :: cctki_use_Kasner_like__q = kind(Kasner_like_
     &_q)
      REAL*8 Kerr_BoyerLindquist__mass
      integer, parameter :: cctki_use_Kerr_BoyerLindquist__mass = kind(K
     &err_BoyerLindquist__mass)
      REAL*8 Kerr_BoyerLindquist__spin
      integer, parameter :: cctki_use_Kerr_BoyerLindquist__spin = kind(K
     &err_BoyerLindquist__spin)
      REAL*8 Kerr_KerrSchild__boost_v
      integer, parameter :: cctki_use_Kerr_KerrSchild__boost_v = kind(Ke
     &rr_KerrSchild__boost_v)
      REAL*8 Kerr_KerrSchild__epsilon
      integer, parameter :: cctki_use_Kerr_KerrSchild__epsilon = kind(Ke
     &rr_KerrSchild__epsilon)
      REAL*8 Kerr_KerrSchild__mass
      integer, parameter :: cctki_use_Kerr_KerrSchild__mass = kind(Kerr_
     &KerrSchild__mass)
      REAL*8 Kerr_KerrSchild__spin
      integer, parameter :: cctki_use_Kerr_KerrSchild__spin = kind(Kerr_
     &KerrSchild__spin)
      REAL*8 Kerr_KerrSchild__t
      integer, parameter :: cctki_use_Kerr_KerrSchild__t = kind(Kerr_Ker
     &rSchild__t)
      REAL*8 Kerr_KerrSchild__x
      integer, parameter :: cctki_use_Kerr_KerrSchild__x = kind(Kerr_Ker
     &rSchild__x)
      REAL*8 Kerr_KerrSchild__y
      integer, parameter :: cctki_use_Kerr_KerrSchild__y = kind(Kerr_Ker
     &rSchild__y)
      REAL*8 Kerr_KerrSchild__z
      integer, parameter :: cctki_use_Kerr_KerrSchild__z = kind(Kerr_Ker
     &rSchild__z)
      REAL*8 Lemaitre__Lambda
      integer, parameter :: cctki_use_Lemaitre__Lambda = kind(Lemaitre__
     &Lambda)
      REAL*8 Lemaitre__R0
      integer, parameter :: cctki_use_Lemaitre__R0 = kind(Lemaitre__R0)
      REAL*8 Lemaitre__epsilon0
      integer, parameter :: cctki_use_Lemaitre__epsilon0 = kind(Lemaitre
     &__epsilon0)
      REAL*8 Lemaitre__kappa
      integer, parameter :: cctki_use_Lemaitre__kappa = kind(Lemaitre__k
     &appa)
      REAL*8 Minkowski_conf_wave__amplitude
      integer, parameter :: cctki_use_Minkowski_conf_wave__amplitude = k
     &ind(Minkowski_conf_wave__amplitude)
      REAL*8 Minkowski_conf_wave__wavelength
      integer, parameter :: cctki_use_Minkowski_conf_wave__wavelength = 
     &kind(Minkowski_conf_wave__wavelength)
      REAL*8 Minkowski_funny__amplitude
      integer, parameter :: cctki_use_Minkowski_funny__amplitude = kind(
     &Minkowski_funny__amplitude)
      REAL*8 Minkowski_funny__sigma
      integer, parameter :: cctki_use_Minkowski_funny__sigma = kind(Mink
     &owski_funny__sigma)
      REAL*8 Minkowski_gauge_wave__amplitude
      integer, parameter :: cctki_use_Minkowski_gauge_wave__amplitude = 
     &kind(Minkowski_gauge_wave__amplitude)
      REAL*8 Minkowski_gauge_wave__lambda
      integer, parameter :: cctki_use_Minkowski_gauge_wave__lambda = kin
     &d(Minkowski_gauge_wave__lambda)
      REAL*8 Minkowski_gauge_wave__omega
      integer, parameter :: cctki_use_Minkowski_gauge_wave__omega = kind
     &(Minkowski_gauge_wave__omega)
      REAL*8 Minkowski_gauge_wave__phase
      integer, parameter :: cctki_use_Minkowski_gauge_wave__phase = kind
     &(Minkowski_gauge_wave__phase)
      REAL*8 Minkowski_shift__amplitude
      integer, parameter :: cctki_use_Minkowski_shift__amplitude = kind(
     &Minkowski_shift__amplitude)
      REAL*8 Minkowski_shift__sigma
      integer, parameter :: cctki_use_Minkowski_shift__sigma = kind(Mink
     &owski_shift__sigma)
      REAL*8 Schwarzschild_BL__epsilon
      integer, parameter :: cctki_use_Schwarzschild_BL__epsilon = kind(S
     &chwarzschild_BL__epsilon)
      REAL*8 Schwarzschild_BL__mass
      integer, parameter :: cctki_use_Schwarzschild_BL__mass = kind(Schw
     &arzschild_BL__mass)
      REAL*8 Schwarzschild_EF__epsilon
      integer, parameter :: cctki_use_Schwarzschild_EF__epsilon = kind(S
     &chwarzschild_EF__epsilon)
      REAL*8 Schwarzschild_EF__mass
      integer, parameter :: cctki_use_Schwarzschild_EF__mass = kind(Schw
     &arzschild_EF__mass)
      REAL*8 Schwarzschild_Lemaitre__Lambda
      integer, parameter :: cctki_use_Schwarzschild_Lemaitre__Lambda = k
     &ind(Schwarzschild_Lemaitre__Lambda)
      REAL*8 Schwarzschild_Lemaitre__mass
      integer, parameter :: cctki_use_Schwarzschild_Lemaitre__mass = kin
     &d(Schwarzschild_Lemaitre__mass)
      REAL*8 Schwarzschild_Novikov__epsilon
      integer, parameter :: cctki_use_Schwarzschild_Novikov__epsilon = k
     &ind(Schwarzschild_Novikov__epsilon)
      REAL*8 Schwarzschild_Novikov__mass
      integer, parameter :: cctki_use_Schwarzschild_Novikov__mass = kind
     &(Schwarzschild_Novikov__mass)
      REAL*8 Schwarzschild_PG__epsilon
      integer, parameter :: cctki_use_Schwarzschild_PG__epsilon = kind(S
     &chwarzschild_PG__epsilon)
      REAL*8 Schwarzschild_PG__mass
      integer, parameter :: cctki_use_Schwarzschild_PG__mass = kind(Schw
     &arzschild_PG__mass)
      REAL*8 Thorne_fakebinary__Omega0
      integer, parameter :: cctki_use_Thorne_fakebinary__Omega0 = kind(T
     &horne_fakebinary__Omega0)
      REAL*8 Thorne_fakebinary__epsilon
      integer, parameter :: cctki_use_Thorne_fakebinary__epsilon = kind(
     &Thorne_fakebinary__epsilon)
      REAL*8 Thorne_fakebinary__mass
      integer, parameter :: cctki_use_Thorne_fakebinary__mass = kind(Tho
     &rne_fakebinary__mass)
      REAL*8 Thorne_fakebinary__separation
      integer, parameter :: cctki_use_Thorne_fakebinary__separation = ki
     &nd(Thorne_fakebinary__separation)
      REAL*8 Thorne_fakebinary__smoothing
      integer, parameter :: cctki_use_Thorne_fakebinary__smoothing = kin
     &d(Thorne_fakebinary__smoothing)
      REAL*8 anti_de_Sitter_Lambda__scale
      integer, parameter :: cctki_use_anti_de_Sitter_Lambda__scale = kin
     &d(anti_de_Sitter_Lambda__scale)
      REAL*8 boost_rotation_symmetric__amp
      integer, parameter :: cctki_use_boost_rotation_symmetric__amp = ki
     &nd(boost_rotation_symmetric__amp)
      REAL*8 boost_rotation_symmetric__min_d
      integer, parameter :: cctki_use_boost_rotation_symmetric__min_d = 
     &kind(boost_rotation_symmetric__min_d)
      REAL*8 boost_rotation_symmetric__scale
      integer, parameter :: cctki_use_boost_rotation_symmetric__scale = 
     &kind(boost_rotation_symmetric__scale)
      REAL*8 bowl__center
      integer, parameter :: cctki_use_bowl__center = kind(bowl__center)
      REAL*8 bowl__sigma
      integer, parameter :: cctki_use_bowl__sigma = kind(bowl__sigma)
      REAL*8 bowl__sigma_t
      integer, parameter :: cctki_use_bowl__sigma_t = kind(bowl__sigma_t
     &)
      REAL*8 bowl__strength
      integer, parameter :: cctki_use_bowl__strength = kind(bowl__streng
     &th)
      REAL*8 bowl__t0
      integer, parameter :: cctki_use_bowl__t0 = kind(bowl__t0)
      REAL*8 bowl__x_scale
      integer, parameter :: cctki_use_bowl__x_scale = kind(bowl__x_scale
     &)
      REAL*8 bowl__y_scale
      integer, parameter :: cctki_use_bowl__y_scale = kind(bowl__y_scale
     &)
      REAL*8 bowl__z_scale
      integer, parameter :: cctki_use_bowl__z_scale = kind(bowl__z_scale
     &)
      REAL*8 constant_density_star__mass
      integer, parameter :: cctki_use_constant_density_star__mass = kind
     &(constant_density_star__mass)
      REAL*8 constant_density_star__radius
      integer, parameter :: cctki_use_constant_density_star__radius = ki
     &nd(constant_density_star__radius)
      REAL*8 de_Sitter_Lambda__scale
      integer, parameter :: cctki_use_de_Sitter_Lambda__scale = kind(de_
     &Sitter_Lambda__scale)
      REAL*8 de_Sitter__scale
      integer, parameter :: cctki_use_de_Sitter__scale = kind(de_Sitter_
     &_scale)
      REAL*8 multi_BH__Hubble
      integer, parameter :: cctki_use_multi_BH__Hubble = kind(multi_BH__
     &Hubble)
      REAL*8 multi_BH__mass1
      integer, parameter :: cctki_use_multi_BH__mass1 = kind(multi_BH__m
     &ass1)
      REAL*8 multi_BH__mass2
      integer, parameter :: cctki_use_multi_BH__mass2 = kind(multi_BH__m
     &ass2)
      REAL*8 multi_BH__mass3
      integer, parameter :: cctki_use_multi_BH__mass3 = kind(multi_BH__m
     &ass3)
      REAL*8 multi_BH__mass4
      integer, parameter :: cctki_use_multi_BH__mass4 = kind(multi_BH__m
     &ass4)
      REAL*8 multi_BH__x1
      integer, parameter :: cctki_use_multi_BH__x1 = kind(multi_BH__x1)
      REAL*8 multi_BH__x2
      integer, parameter :: cctki_use_multi_BH__x2 = kind(multi_BH__x2)
      REAL*8 multi_BH__x3
      integer, parameter :: cctki_use_multi_BH__x3 = kind(multi_BH__x3)
      REAL*8 multi_BH__x4
      integer, parameter :: cctki_use_multi_BH__x4 = kind(multi_BH__x4)
      REAL*8 multi_BH__y1
      integer, parameter :: cctki_use_multi_BH__y1 = kind(multi_BH__y1)
      REAL*8 multi_BH__y2
      integer, parameter :: cctki_use_multi_BH__y2 = kind(multi_BH__y2)
      REAL*8 multi_BH__y3
      integer, parameter :: cctki_use_multi_BH__y3 = kind(multi_BH__y3)
      REAL*8 multi_BH__y4
      integer, parameter :: cctki_use_multi_BH__y4 = kind(multi_BH__y4)
      REAL*8 multi_BH__z1
      integer, parameter :: cctki_use_multi_BH__z1 = kind(multi_BH__z1)
      REAL*8 multi_BH__z2
      integer, parameter :: cctki_use_multi_BH__z2 = kind(multi_BH__z2)
      REAL*8 multi_BH__z3
      integer, parameter :: cctki_use_multi_BH__z3 = kind(multi_BH__z3)
      REAL*8 multi_BH__z4
      integer, parameter :: cctki_use_multi_BH__z4 = kind(multi_BH__z4)
      integer*8 Minkowski_gauge_wave__what_fn
      integer, parameter :: cctki_use_Minkowski_gauge_wave__what_fn = ki
     &nd(Minkowski_gauge_wave__what_fn)
      integer*8 Thorne_fakebinary__atype
      integer, parameter :: cctki_use_Thorne_fakebinary__atype = kind(Th
     &orne_fakebinary__atype)
      integer*8 bowl__shape
      integer, parameter :: cctki_use_bowl__shape = kind(bowl__shape)
      integer*8 exact_model
      integer, parameter :: cctki_use_exact_model = kind(exact_model)
      INTEGER*4 Kerr_KerrSchild__parabolic
      integer, parameter :: cctki_use_Kerr_KerrSchild__parabolic = kind(
     &Kerr_KerrSchild__parabolic)
      INTEGER*4 Kerr_KerrSchild__power
      integer, parameter :: cctki_use_Kerr_KerrSchild__power = kind(Kerr
     &_KerrSchild__power)
      INTEGER*4 Minkowski_conf_wave__direction
      integer, parameter :: cctki_use_Minkowski_conf_wave__direction = k
     &ind(Minkowski_conf_wave__direction)
      INTEGER*4 Minkowski_gauge_wave__diagonal
      integer, parameter :: cctki_use_Minkowski_gauge_wave__diagonal = k
     &ind(Minkowski_gauge_wave__diagonal)
      INTEGER*4 Thorne_fakebinary__retarded
      integer, parameter :: cctki_use_Thorne_fakebinary__retarded = kind
     &(Thorne_fakebinary__retarded)
      INTEGER*4 bowl__evolve
      integer, parameter :: cctki_use_bowl__evolve = kind(bowl__evolve)
      INTEGER*4 multi_BH__nBH
      integer, parameter :: cctki_use_multi_BH__nBH = kind(multi_BH__nBH
     &)
      COMMON /exactrest/Alvi__mass1, Alvi__mass2, Alvi__separation, Bert
     &otti__Lambda, Bianchi_I__scale, Goedel__scale, Gowdy_wave__amplitu
     &de, Kasner_generalized__p1, Kasner_generalized__p2, Kasner_like__q
     &, Kerr_BoyerLindquist__mass, Kerr_BoyerLindquist__spin, Kerr_KerrS
     &child__boost_v, Kerr_KerrSchild__epsilon, Kerr_KerrSchild__mass, K
     &err_KerrSchild__spin, Kerr_KerrSchild__t, Kerr_KerrSchild__x, Kerr
     &_KerrSchild__y, Kerr_KerrSchild__z, Lemaitre__Lambda, Lemaitre__R0
     &, Lemaitre__epsilon0, Lemaitre__kappa, Minkowski_conf_wave__amplit
     &ude, Minkowski_conf_wave__wavelength, Minkowski_funny__amplitude, 
     &Minkowski_funny__sigma, Minkowski_gauge_wave__amplitude, Minkowski
     &_gauge_wave__lambda, Minkowski_gauge_wave__omega, Minkowski_gauge_
     &wave__phase, Minkowski_shift__amplitude, Minkowski_shift__sigma, S
     &chwarzschild_BL__epsilon, Schwarzschild_BL__mass, Schwarzschild_EF
     &__epsilon, Schwarzschild_EF__mass, Schwarzschild_Lemaitre__Lambda,
     & Schwarzschild_Lemaitre__mass, Schwarzschild_Novikov__epsilon, Sch
     &warzschild_Novikov__mass, Schwarzschild_PG__epsilon, Schwarzschild
     &_PG__mass, Thorne_fakebinary__Omega0, Thorne_fakebinary__epsilon, 
     &Thorne_fakebinary__mass, Thorne_fakebinary__separation, Thorne_fak
     &ebinary__smoothing, anti_de_Sitter_Lambda__scale, boost_rotation_s
     &ymmetric__amp, boost_rotation_symmetric__min_d, boost_rotation_sym
     &metric__scale, bowl__center, bowl__sigma, bowl__sigma_t, bowl__str
     &ength, bowl__t0, bowl__x_scale, bowl__y_scale, bowl__z_scale, cons
     &tant_density_star__mass, constant_density_star__radius, de_Sitter_
     &Lambda__scale, de_Sitter__scale, multi_BH__Hubble, multi_BH__mass1
     &, multi_BH__mass2, multi_BH__mass3, multi_BH__mass4, multi_BH__x1,
     & multi_BH__x2, multi_BH__x3, multi_BH__x4, multi_BH__y1, multi_BH_
     &_y2, multi_BH__y3, multi_BH__y4, multi_BH__z1, multi_BH__z2, multi
     &_BH__z3, multi_BH__z4, Minkowski_gauge_wave__what_fn, Thorne_fakeb
     &inary__atype, bowl__shape, exact_model, Kerr_KerrSchild__parabolic
     &, Kerr_KerrSchild__power, Minkowski_conf_wave__direction, Minkowsk
     &i_gauge_wave__diagonal, Thorne_fakebinary__retarded, bowl__evolve,
     & multi_BH__nBH
      REAL*8 boost_vx
      integer, parameter :: cctki_use_boost_vx = kind(boost_vx)
      REAL*8 boost_vy
      integer, parameter :: cctki_use_boost_vy = kind(boost_vy)
      REAL*8 boost_vz
      integer, parameter :: cctki_use_boost_vz = kind(boost_vz)
      REAL*8 exact_eps
      integer, parameter :: cctki_use_exact_eps = kind(exact_eps)
      REAL*8 exblend_rout
      integer, parameter :: cctki_use_exblend_rout = kind(exblend_rout)
      REAL*8 exblend_width
      integer, parameter :: cctki_use_exblend_width = kind(exblend_width
     &)
      REAL*8 rotation_euler_phi
      integer, parameter :: cctki_use_rotation_euler_phi = kind(rotation
     &_euler_phi)
      REAL*8 rotation_euler_psi
      integer, parameter :: cctki_use_rotation_euler_psi = kind(rotation
     &_euler_psi)
      REAL*8 rotation_euler_theta
      integer, parameter :: cctki_use_rotation_euler_theta = kind(rotati
     &on_euler_theta)
      REAL*8 shift_add_x
      integer, parameter :: cctki_use_shift_add_x = kind(shift_add_x)
      REAL*8 shift_add_y
      integer, parameter :: cctki_use_shift_add_y = kind(shift_add_y)
      REAL*8 shift_add_z
      integer, parameter :: cctki_use_shift_add_z = kind(shift_add_z)
      REAL*8 slice_Gauss_ampl
      integer, parameter :: cctki_use_slice_Gauss_ampl = kind(slice_Gaus
     &s_ampl)
      REAL*8 slice_Gauss_width
      integer, parameter :: cctki_use_slice_Gauss_width = kind(slice_Gau
     &ss_width)
      integer*8 overwrite_boundary
      integer, parameter :: cctki_use_overwrite_boundary = kind(overwrit
     &e_boundary)
      INTEGER*4 exact_order
      integer, parameter :: cctki_use_exact_order = kind(exact_order)
      INTEGER*4 exblend_Ks
      integer, parameter :: cctki_use_exblend_Ks = kind(exblend_Ks)
      INTEGER*4 exblend_gauge
      integer, parameter :: cctki_use_exblend_gauge = kind(exblend_gauge
     &)
      INTEGER*4 exblend_gs
      integer, parameter :: cctki_use_exblend_gs = kind(exblend_gs)
      COMMON /Exactpriv/boost_vx, boost_vy, boost_vz, exact_eps, exblend
     &_rout, exblend_width, rotation_euler_phi, rotation_euler_psi, rota
     &tion_euler_theta, shift_add_x, shift_add_y, shift_add_z, slice_Gau
     &ss_ampl, slice_Gauss_width, overwrite_boundary, exact_order, exble
     &nd_Ks, exblend_gauge, exblend_gs
      integer*8 CCTKH0
      integer, parameter :: cctki_use_CCTKH0 = kind(CCTKH0)
      integer*8 dtlapse_evolution_method
      integer, parameter :: cctki_use_dtlapse_evolution_method = kind(dt
     &lapse_evolution_method)
      integer*8 dtshift_evolution_method
      integer, parameter :: cctki_use_dtshift_evolution_method = kind(dt
     &shift_evolution_method)
      integer*8 evolution_method
      integer, parameter :: cctki_use_evolution_method = kind(evolution_
     &method)
      integer*8 initial_data
      integer, parameter :: cctki_use_initial_data = kind(initial_data)
      integer*8 initial_dtlapse
      integer, parameter :: cctki_use_initial_dtlapse = kind(initial_dtl
     &apse)
      integer*8 initial_dtshift
      integer, parameter :: cctki_use_initial_dtshift = kind(initial_dts
     &hift)
      integer*8 initial_lapse
      integer, parameter :: cctki_use_initial_lapse = kind(initial_lapse
     &)
      integer*8 initial_shift
      integer, parameter :: cctki_use_initial_shift = kind(initial_shift
     &)
      integer*8 lapse_evolution_method
      integer, parameter :: cctki_use_lapse_evolution_method = kind(laps
     &e_evolution_method)
      integer*8 CCTKH1
      integer, parameter :: cctki_use_CCTKH1 = kind(CCTKH1)
      integer*8 CCTKH3
      integer, parameter :: cctki_use_CCTKH3 = kind(CCTKH3)
      integer*8 metric_type
      integer, parameter :: cctki_use_metric_type = kind(metric_type)
      integer*8 shift_evolution_method
      integer, parameter :: cctki_use_shift_evolution_method = kind(shif
     &t_evolution_method)
      integer*8 CCTKH5
      integer, parameter :: cctki_use_CCTKH5 = kind(CCTKH5)
      INTEGER*4 CCTKH2
      integer, parameter :: cctki_use_CCTKH2 = kind(CCTKH2)
      INTEGER*4 CCTKH4
      integer, parameter :: cctki_use_CCTKH4 = kind(CCTKH4)
      INTEGER*4 CCTKH6
      integer, parameter :: cctki_use_CCTKH6 = kind(CCTKH6)
      COMMON /ADMBASErest/CCTKH0, dtlapse_evolution_method, dtshift_evol
     &ution_method, evolution_method, initial_data, initial_dtlapse, ini
     &tial_dtshift, initial_lapse, initial_shift, lapse_evolution_method
     &, CCTKH1, CCTKH3, metric_type, shift_evolution_method, CCTKH5, CCT
     &KH2, CCTKH4, CCTKH6
      integer*8 conformal_storage
      integer, parameter :: cctki_use_conformal_storage = kind(conformal
     &_storage)
      COMMON /STATICCONFORMALrest/conformal_storage
	interface
      integer*8 function CCTK_PointerTo (var)
      implicit none
      type(*),dimension(..),TARGET :: var
      end function CCTK_PointerTo
      end interface
      interface
      integer function CCTK_Equals (arg1, arg2)
      implicit none
      integer*8 arg1
      character(*) arg2
      end function CCTK_Equals
      integer function CCTK_MyProc (cctkGH)
      implicit none
      integer*8 cctkGH
      end function CCTK_MyProc
      integer function CCTK_nProcs (cctkGH)
      implicit none
      integer*8 cctkGH
      end function CCTK_nProcs
      integer function CCTK_IsThornActive (name)
      implicit none
      character(*) name
      end function CCTK_IsThornActive
      integer*8 function CCTK_NullPointer ()
      implicit none
      end function CCTK_NullPointer
      end interface
      interface
      INTEGER*4 function Boundary_RegisterPhysicalBC (GH, function_point
     &er, bc_name)
      implicit none
      integer*8 GH
      external function_pointer
      INTEGER*4 function_pointer
      character(*) bc_name
      end function Boundary_RegisterPhysicalBC
      end interface
      interface
      INTEGER*4 function Boundary_SelectGroupForBC (GH, faces, boundary_
     &width, table_handle, group_name, bc_name)
      implicit none
      integer*8 GH
      INTEGER*4 faces
      INTEGER*4 boundary_width
      INTEGER*4 table_handle
      character(*) group_name
      character(*) bc_name
      end function Boundary_SelectGroupForBC
      end interface
      interface
      INTEGER*4 function Driver_NotifyDataModified (cctkGH, variables, t
     &ls, nvariables, where)
      implicit none
      integer*8 cctkGH
      INTEGER*4 variables(*)
      INTEGER*4 tls(*)
      INTEGER*4 nvariables
      INTEGER*4 where(*)
      end function Driver_NotifyDataModified
      end interface
      interface
      INTEGER*4 function Driver_RequireValidData (cctkGH, variables, tls
     &, nvariables, where)
      implicit none
      integer*8 cctkGH
      INTEGER*4 variables(*)
      INTEGER*4 tls(*)
      INTEGER*4 nvariables
      INTEGER*4 where(*)
      end function Driver_RequireValidData
      end interface

c

c
c
c

c
	if      (CCTK_Equals(exact_model, "Minkowski") .ne. 0) then
	     decoded_exact_model = 1
	elseif (CCTK_Equals(exact_model, "Minkowski/shift") .ne. 0) then
	     decoded_exact_model = 2
	elseif (CCTK_Equals(exact_model, "Minkowski/funny") .ne. 0) then
	     decoded_exact_model = 3
	elseif (CCTK_Equals(exact_model, "Minkowski/gauge wave") .ne. 0) then
	     decoded_exact_model = 4
	elseif (CCTK_Equals(exact_model, "Minkowski/shifted gauge wave") .ne. 0
     &) then
	     decoded_exact_model = 5
	elseif (CCTK_Equals(exact_model, "Minkowski/conf wave") .ne. 0) then
	     decoded_exact_model = 6

c
	elseif (CCTK_Equals(exact_model, "Schwarzschild/EF") .ne. 0) then
	     decoded_exact_model = 10
	elseif (CCTK_Equals(exact_model, "Schwarzschild/PG") .ne. 0) then
	     decoded_exact_model = 11
	elseif (CCTK_Equals(exact_model, "Schwarzschild/BL") .ne. 0) then
	     decoded_exact_model = 12
	elseif (CCTK_Equals(exact_model, "Schwarzschild/Novikov") .ne. 0) then
	     decoded_exact_model = 13
	elseif (CCTK_Equals(exact_model, "Kerr/Boyer-Lindquist") .ne. 0) then
	     decoded_exact_model = 14
	elseif (CCTK_Equals(exact_model, "Kerr/Kerr-Schild") .ne. 0) then
	     decoded_exact_model = 15
	elseif (CCTK_Equals(exact_model, "Kerr/Kerr-Schild/spherical") .ne. 0) 
     &then
	     decoded_exact_model = 16
	elseif (CCTK_Equals(exact_model, "Schwarzschild-Lemaitre") .ne. 0) then
     &
	     decoded_exact_model = 17
	elseif (CCTK_Equals(exact_model, "multi-BH") .ne. 0) then
	     decoded_exact_model = 18
	elseif (CCTK_Equals(exact_model, "Alvi") .ne. 0) then
	     decoded_exact_model = 19
	elseif (CCTK_Equals(exact_model, "Thorne-fakebinary") .ne. 0) then
	     decoded_exact_model = 20

c
	elseif (CCTK_Equals(exact_model, "Lemaitre") .ne. 0) then
	     decoded_exact_model = 50
C this metric doesnt work and has been moved to ../archive/
CC	elseif (CCTK_Equals(exact_model, "Robertson-Walker") .ne. 0) then
CC	     decoded_exact_model = EXACT__Robertson_Walker
	elseif (CCTK_Equals(exact_model, "de Sitter") .ne. 0) then
	     decoded_exact_model = 52
	elseif (CCTK_Equals(exact_model, "de Sitter+Lambda") .ne. 0) then
	     decoded_exact_model = 53
	elseif (CCTK_Equals(exact_model, "anti-de Sitter+Lambda") .ne. 0) then
	     decoded_exact_model = 54
	elseif (CCTK_Equals(exact_model, "Bianchi I") .ne. 0) then
	     decoded_exact_model = 55
	elseif (CCTK_Equals(exact_model, "Goedel") .ne. 0) then
	     decoded_exact_model = 56
	elseif (CCTK_Equals(exact_model, "Bertotti") .ne. 0) then
	     decoded_exact_model = 57
	elseif (CCTK_Equals(exact_model, "Kasner-like") .ne. 0) then
	     decoded_exact_model = 58
	elseif (CCTK_Equals(exact_model, "Kasner-axisymmetric") .ne. 0) then
	     decoded_exact_model = 59
	elseif (CCTK_Equals(exact_model, "Kasner-generalized") .ne. 0) then
	     decoded_exact_model = 60
	elseif (CCTK_Equals(exact_model, "Gowdy-wave") .ne. 0) then
	     decoded_exact_model = 61
	elseif (CCTK_Equals(exact_model, "Milne") .ne. 0) then
	     decoded_exact_model = 62

c
	elseif (CCTK_Equals(exact_model, "boost-rotation symmetric") .ne. 0) th
     &en
	     decoded_exact_model = 80
	elseif (CCTK_Equals(exact_model, "bowl") .ne. 0) then
	     decoded_exact_model = 81
	elseif (CCTK_Equals(exact_model, "constant density star") .ne. 0) then
	     decoded_exact_model = 82
	else
	     call CCTK_Warn(0,127,"decode_pars.F","Exact", "Unknown exact_model
     &")
	endif

c

c
c
c
c
	Schwarzschild_Lemaitre___Lambda = Schwarzschild_Lemaitre__Lambda
	Schwarzschild_Lemaitre___mass   = Schwarzschild_Lemaitre__mass

c

c
c
c
	Lemaitre___kappa    = Lemaitre__kappa
	Lemaitre___Lambda   = Lemaitre__Lambda
	Lemaitre___epsilon0 = Lemaitre__epsilon0
	Lemaitre___R0       = Lemaitre__R0

c
CCC
CCC this metric doesnt work and has been moved to ../archive/
CCc
CCc parameters for Robertson-Walker spacetime
CCc
CC	Robertson_Walker___R0       = Robertson_Walker__R0
CC	Robertson_Walker___rho      = Robertson_Walker__rho
CC	Robertson_Walker___k        = Robertson_Walker__k
CC	Robertson_Walker___pressure = Robertson_Walker__pressure
CC
c

c
c
c
	de_Sitter___scale = de_Sitter__scale

c

c
c
c
	de_Sitter_Lambda___scale = de_Sitter_Lambda__scale

c

c
c
c
	anti_de_Sitter_Lambda___scale = anti_de_Sitter_Lambda__scale

c

c
c
c
	Bertotti___Lambda = Bertotti__Lambda

c

c
c
c
	Kasner_like___q = Kasner_like__q

c

c
c
c
	Kasner_generalized___p1 = Kasner_generalized__p1
	Kasner_generalized___p2 = Kasner_generalized__p2

c

c
c
c
	constant_density_star___mass   = constant_density_star__mass
	constant_density_star___radius = constant_density_star__radius

c

	return
	end
