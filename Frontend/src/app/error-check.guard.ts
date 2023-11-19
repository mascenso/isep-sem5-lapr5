import { Injectable } from '@angular/core';
import { CanActivate, ActivatedRouteSnapshot, RouterStateSnapshot, Router } from '@angular/router';
import { catchError, Observable } from "rxjs";
import { BridgeService } from "./services/bridge.service";
import { MatSnackBar } from "@angular/material/snack-bar";

@Injectable({
  providedIn: 'root',
})
export class ErrorCheckGuard implements CanActivate {
  constructor(private router: Router, private myService: BridgeService, private _snackBar: MatSnackBar)  {

  }

  canActivate(route: ActivatedRouteSnapshot, state: RouterStateSnapshot): boolean {
    let hasError: boolean = false;

    this.myService.getAllBuildings().pipe(
      catchError((error) => {
        console.log('ErrorCheckGuard: Error detected, redirecting to /home/campus/');
        hasError = true;
        //snack bar
        throw error; // Re-throw the error so it's caught by the next error handler
      })
    ).subscribe((buildings) => {
      if (buildings && Array.isArray(buildings) && buildings.length === 0) {
        console.error('ErrorCheckGuard: No buildings found, triggering error...');
        this._snackBar.open("No buildings found", "close", {
          duration: 5000,
          panelClass: ['snackbar-warning']
        });
        hasError = true;
      }

      if (hasError) {
        console.log('ErrorCheckGuard: Error detected, redirecting to /home/campus/');
        // Prevent navigation and redirect to a different page (e.g., an error page)
        this.router.navigate(['/home/campus/']);
      }
    });

    // Always return true here, as the actual decision is made in the subscribe callback
    return true;
  }
}
