import { Component } from '@angular/core';
import {LoadingSpinnerService} from "../services/loading-spinner.service";

@Component({
  selector: 'app-home',
  templateUrl: './home.component.html',
  styleUrls: ['./home.component.css']
})
export class HomeComponent {
  isExpanded: boolean = false;

  constructor(public spinnerService: LoadingSpinnerService) {}
}
