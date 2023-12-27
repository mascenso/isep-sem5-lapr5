import { Component } from '@angular/core';
import {Router} from "@angular/router";
import {AuthService} from "../services/auth.service";
import {FormControl, FormGroup, Validators} from "@angular/forms";

@Component({
  selector: 'app-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.css']
})
export class LoginComponent {
  hidePassword=true;

  loginForm = new FormGroup({
    email: new FormControl('', [Validators.required, Validators.email]),
    password: new FormControl('', [Validators.required]),
  });

  constructor(private authService: AuthService,
              private router: Router) {}

  onSubmit(event:Event){
    event.preventDefault();
    this.onLogin(event);
  }

  onLogin(event:Event) {
    if (!this.loginForm.invalid) {
      this.authService.login(this.loginForm.controls.email.value!, this.loginForm.controls.password.value!, '').subscribe(
        result => {
          if (result != null) {
            localStorage.setItem('token', Math.random().toString());
            localStorage.setItem('role', result.role);
            this.router.navigate(['home']);
          }
        }
      )
    }
  }

  getErrorMessage() {
    if (this.loginForm.controls.email.hasError('required')) {
      return 'You must enter a value';
    }

    return this.loginForm.controls.email.hasError('email') ? 'Not a valid email' : '';
  }

}
