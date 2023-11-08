import { Component } from '@angular/core';
import { loginService } from '../services/loginService.service';

@Component({
  selector: 'app-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.css'],
  providers: [loginService]
})
export class LoginComponent {
  email: string = '';
  password: string = '';
  selectedRole: string = '';
  roles: any[] = [];

  constructor(private loginService: loginService) {}

  ngOnInit() {
    this.loginService.getRoles().subscribe(roles => {
      this.roles = roles;
    });
  }

  onSubmit(event:Event,email:string,password:string,role:string){
    event.preventDefault();

  }


}
