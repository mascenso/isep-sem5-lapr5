import { ComponentFixture, TestBed } from '@angular/core/testing';
import { MatSnackBar } from '@angular/material/snack-bar';
import { MatTableModule } from '@angular/material/table';
import { Router } from '@angular/router';
import { MatIconModule } from '@angular/material/icon';
import { RouterTestingModule } from '@angular/router/testing';
import { of } from 'rxjs';
import { UserResponseDTO } from "src/dto/userDTO";
import { ValidateUserComponent } from 'src/app/home/users/validate-user/validate-user.component';
import { UserService } from 'src/app/services/user.service';

describe('ValidateUserComponent', () => {
  let component: ValidateUserComponent;
  let fixture: ComponentFixture<ValidateUserComponent>;
  
  // Mock services
  const mockUserService = {
    GetInactiveUsers: jasmine.createSpy().and.returnValue(of([])),
    updateUserById: jasmine.createSpy().and.returnValue(of([])),
    deleteUserById: jasmine.createSpy().and.returnValue(of({})),
  };

  const mockSnackBar = {
    open: jasmine.createSpy(),
  };

  const routerSpy = jasmine.createSpyObj('Router', ['navigate']);


  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ValidateUserComponent],
      imports: [RouterTestingModule, MatIconModule, MatTableModule],
      providers: [
        { provide: UserService, useValue: mockUserService },
        { provide: MatSnackBar, useValue: mockSnackBar },
        { provide: Router, useValue: routerSpy },
      ],
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(ValidateUserComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should fetch inactive users on initialization', () => {
    const mockUsers: UserResponseDTO[] = [{
      id: '1',
      email: 'test@example.com',
      password: '12345',
      firstName: 'teste',
      lastName: 'Final',
      active: false,
      role: 'USER',
      taxPayerNumber: '987654321',
      mechanographicNumber: '2100987',
      phoneNumber: '123456789'
    }];
    (mockUserService.GetInactiveUsers as jasmine.Spy).and.callFake(() => {
      return of(mockUsers);
    });

    component.ngOnInit();
  
    expect(mockUserService.GetInactiveUsers).toHaveBeenCalled();
    expect(component.userList).toEqual(mockUsers);
  });

 

  it('should accept a user', () => {
    const userToAccept: UserResponseDTO = {
      id: '1',
      email: 'test@example.com',
      password: 'password',
      firstName: 'John',
      lastName: 'Doe',
      role: 'user',
      active: false,
      taxPayerNumber: '12345',
      mechanographicNumber: '54321',
      phoneNumber: '987654321'
    };

    component.acceptUser(userToAccept);

    expect(mockUserService.updateUserById).toHaveBeenCalledWith(
      '1', // ID
      jasmine.objectContaining({ 
        id: '1',
        email: 'test@example.com',
        firstName: 'John',
        lastName: 'Doe',
        role: 'user',
        active: true,
      })
    );
    expect(mockSnackBar.open).toHaveBeenCalledWith(
      'User approved!', 'close', 
    {
      duration: 5000,
      panelClass: ['snackbar-success']
      // Other configurations you're testing
    });
  });

  it('should reject a user', () => {
    const userToReject: UserResponseDTO = {
      id: '1',
      email: 'test@example.com',
      password: 'password',
      firstName: 'John',
      lastName: 'Doe',
      role: 'user',
      active: false,
      taxPayerNumber: '12345',
      mechanographicNumber: '54321',
      phoneNumber: '987654321'
    }
  
    component.rejectUser(userToReject);
  
    expect(mockUserService.deleteUserById).toHaveBeenCalledWith('1');  
    expect(mockSnackBar.open).toHaveBeenCalledWith("User deleted!", "close", {
      duration: 5000,
      panelClass: ['snackbar-success']
    });
  });
});
