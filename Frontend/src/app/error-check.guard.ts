import { Injectable } from '@angular/core';
import { CanActivate, ActivatedRouteSnapshot, RouterStateSnapshot, Router } from '@angular/router';
import { Observable } from "rxjs";
import { BridgeService } from "./services/bridge.service";

@Injectable({
  providedIn: 'root',
})
export class ErrorCheckGuard implements CanActivate {
  constructor(private router: Router, private myService: BridgeService) {

  }

  canActivate(route: ActivatedRouteSnapshot, state: RouterStateSnapshot): boolean {
    // Check your condition here



    let hasError : boolean = false;

    try {
      console.log(this.myService.getAllBuildings());
      console.log('ErrorCheckGuard: No error detected, allowing navigation to requested route...');
    }
    catch (e) {
      console.log('ErrorCheckGuard: Error detected, redirecting to /home/campus/');
      hasError = true;
    }

    if (hasError) {
      console.log('ErrorCheckGuard: Error detected, redirecting to /home/campus/');
      // Prevent navigation and redirect to a different page (e.g., an error page)
      console.log('ErrorCheckGuard: Error detected, redirecting to /home/campus/');
      this.router.navigate(['/home/campus/']);
      return false;
    }

    // Allow navigation if no error
    return true;
  }
}
