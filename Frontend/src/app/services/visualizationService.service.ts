import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable, throwError } from 'rxjs';
import { catchError } from 'rxjs/operators';

@Injectable({
  providedIn: 'root'
})
export class VisualizationService {
  constructor(private http: HttpClient) {}

  getJsonData(jsonPath: string): Observable<any> {
    return this.http.get(jsonPath).pipe(
      catchError((error: any) => {
        console.error('Error loading JSON file:', error);
        return throwError('Error loading JSON file');
      })
    );
  }
}