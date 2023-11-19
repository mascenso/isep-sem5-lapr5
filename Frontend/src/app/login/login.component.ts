import { Component } from '@angular/core';
import {Router} from "@angular/router";
import UserRole from "../../../../Gestao_Informacao/src/enums/userRole";
import {AuthService} from "../services/auth.service";

@Component({
  selector: 'app-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.css']
})
export class LoginComponent {
  email: string = '';
  password: string = '';
  selectedRole: string = '';
  roles= Object.values(UserRole);

  constructor(private authService: AuthService,
              private router: Router) {}

  onSubmit(event:Event,email:string,password:string,role:string){
    event.preventDefault();
    this.onLogin(event, email, password, role);
  }

  onLogin(event:Event,email:string,password:string,role:string) {
    this.authService.login(email, password, role).subscribe(
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
