import { ErrorHandler, Inject, Injectable, Injector } from "@angular/core";
import { HttpErrorResponse } from "@angular/common/http";
import { MatSnackBar } from "@angular/material/snack-bar";
import { ErrorsConstant } from "./errors-const";

@Injectable()
export class GlobalErrorHandler implements ErrorHandler {

  errorMsg = '';
  constructor(@Inject(Injector) private injector: Injector) {
  }

  private get snackBarError(): MatSnackBar {
    return this.injector.get(MatSnackBar);
  }

  // Handle API errors
  handleError(error: HttpErrorResponse): void {
    console.log(
      `Error code ${error.status}, ` +
      `body was: ${error.error}`);
    if (!navigator.onLine) {
      this.errorMsg = ErrorsConstant.NetworkIssue;
    } else if (error.status >= 400 && error.status <= 499) {
      this.errorMsg = ErrorsConstant.ClientSide;
    } else {
      this.errorMsg = ErrorsConstant.ServerSide;
    }
    console.log(this.errorMsg);

    this.snackBarError.open(this.errorMsg, "Fechar", { duration: 3000, panelClass:['snackbar-error'] });
  }
}
