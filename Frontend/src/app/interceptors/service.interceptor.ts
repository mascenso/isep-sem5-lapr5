import { Injectable } from '@angular/core';
import {
  HttpRequest,
  HttpHandler,
  HttpEvent,
  HttpInterceptor
} from '@angular/common/http';
import {finalize, Observable} from 'rxjs';
import {LoadingSpinnerService} from "../services/loading-spinner.service";

@Injectable()
export class ServiceInterceptor implements HttpInterceptor {
  constructor(private spinnerService: LoadingSpinnerService) {}
  intercept(request: HttpRequest<any>, next: HttpHandler): Observable<HttpEvent<any>> {
    if (request.reportProgress) {
      this.spinnerService.loading.next(true);

      return next.handle(request)
        .pipe(
          finalize(() => this.spinnerService.loading.next(false))
        );
    } else {
      return next.handle(request);
    }
  }
}
