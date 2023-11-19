import { Component } from '@angular/core';
import {LoadingSpinnerService} from "../services/loading-spinner.service";
import {AuthService} from "../services/auth.service";

@Component({
  selector: 'app-home',
  templateUrl: './home.component.html',
  styleUrls: ['./home.component.css']
})
export class HomeComponent {
  isExpanded: boolean = false;

  constructor(private authService: AuthService,
              public spinnerService: LoadingSpinnerService) {}

  public onLogout() {
    this.authService.logout();
  }
}
