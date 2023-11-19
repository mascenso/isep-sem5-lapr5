import {CanActivateFn, Router} from '@angular/router';
import {inject} from "@angular/core";
import {AuthService} from "../services/auth.service";

export const authGuard: CanActivateFn = (route, state) => {
  const router: Router = inject(Router);
  const loginService: AuthService = inject(AuthService);
  if (loginService.isLoggedId() && localStorage.getItem('token')) {
    return true
  }
  else {
    router.navigate(['login']);
    return false;
  }
};
