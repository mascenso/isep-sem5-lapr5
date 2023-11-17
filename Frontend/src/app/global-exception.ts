import { ErrorHandler, Injectable, NgZone } from "@angular/core";
import { HttpErrorResponse } from "@angular/common/http";
import { Router } from "@angular/router";
import { MatSnackBar } from "@angular/material/snack-bar";

@Injectable()
export class GlobalErrorHandler implements ErrorHandler {

  constructor(private ngZone: NgZone, private router: Router, private _snackBar: MatSnackBar) { }

  handleError(error: any): void {

    if (error instanceof HttpErrorResponse) {
        if (!navigator.onLine) {
          alert('No Internet Connection');
        } else {
          //snack bar
          this._snackBar.open("Http error", "close", {
            duration: 5000,
            panelClass: ['snackbar-warning']
          });

          this.ngZone.run(() => this.router.navigateByUrl('/home/campus'));
        }
      }
    }

}
