describe('Teste de listar uma tarefas pendentes', () => {
  
    before(() => {
        cy.visit('http://localhost:4200/login');
        cy.get('#login-input-username').type('admin@email.pt');
        cy.get('#login-input-password').type('admin');
        cy.get('button[type="submit"]').click();

        cy.get('[id^="Campus"]').click();
        cy.get('[routerlink="create-building"]').click();
        const numeroAleatorio = Math.floor(1000 + Math.random() * 9000);
        cy.get('#mat-input-9').type('A'+numeroAleatorio.toString());
        cy.get('#mat-input-10').type("10");
        cy.get('#mat-input-11').type('10');
        cy.get('#mat-input-12').type('teste');
        cy.get('#mat-input-13').type('Mais um edificio de teste');
        cy.get('#submitButtonBuilding').click()
        cy.get('[id^="Campus"]').click();
        cy.get('[routerlink="create-floor"]').click();
        cy.get('#dropdownCreateFloor').click().get('mat-option').last().click();
        cy.get('#mat-input-14').type("10");
        cy.get('#mat-input-15').type('10');
        cy.get('#mat-input-16').type('4');
        cy.get('#mat-input-17').type('Mais um floor de teste');
        cy.get('#saveNewFloor').click();

        cy.get('[id^="Tasks"]').click();
        cy.get('[routerlink="create-task"]').click();
        cy.get('#select-task-dropdown').click();
        cy.get('#optionPickup').click();
        cy.get('#pickupTask-dropdownBuildingPickup').click().get('mat-option').last().click();
        cy.get('#pickupTask-dropdownFloorPickup').click().get('mat-option').last().click();
        cy.get('#pickupTask-dropdownBuildingPickup-2').click().get('mat-option').last().click();
        cy.get('#pickupTask-dropdownFloorDelivery2').click().get('mat-option').last().click();
        cy.get('#pickupTaskName').type("Testing Task 1");
        cy.get('#pickupTaskContact').type("10");
        cy.get('#pickupTasklocalizationx2').type("10");
        cy.get('#createTaskPickupDescription').type("Testing Task 1");
        cy.get('#pickupTasksubmitbutton').click();

        cy.get('[id^="Tasks"]').click();
        cy.get('[routerlink="create-task"]').click();
        cy.get('#select-task-dropdown').click();
        cy.get('#optionVigilance').click();
        cy.get('#vigilanceTask-dropdownBuilding').click().get('mat-option').last().click();
        cy.get('#vigilanceTask-dropdownFloor').click().get('mat-option').last().click();
        cy.get('#vigilanceTaskDescription').type("Testing Task 1");
        cy.get('#vigilanceTaskContact').type("10");
        cy.get('#vigilanceTaskstartx').type("10");
        cy.get('#vigilanceTaskstarty').type("10");
        cy.get('#vigilanceTaskendx').type("10");
        cy.get('#vigilanceTaskendy').type("10");
        cy.get('#vigilanceTasksubmitbutton').click()



        cy.get('[id^="Tasks"]').click();
        cy.get('[routerlink="pending-task-list"]').click();

    });
  

      it('Deve exibir a tabela com valores que vai buscar a bd', () => {

        cy.get('table.mat-elevation-z8').should('exist');
        cy.get('.mat-mdc-header-row > .cdk-column-description').should('exist');
        cy.get('.mat-mdc-header-row > .cdk-column-user-name').should('exist');
        cy.get('.mat-mdc-header-row > .cdk-column-user-contact').should('exist');
        cy.get('.mat-mdc-header-row > .cdk-column-type').should('exist');
        cy.get('.mdc-data-table__content > :nth-child(1) > .cdk-column-description').should('not.be.empty');
        cy.get('.mdc-data-table__content > :nth-child(1) > .cdk-column-user-name').should('not.be.empty');
        cy.get('.mdc-data-table__content > :nth-child(1) > .cdk-column-user-contact').should('not.be.empty');
        cy.get('.mdc-data-table__content > :nth-child(1) > .cdk-column-type').should('not.be.empty');
      });

      it('Deve exibir a tabela corretamente e com valores que vai buscar a bd da ultima task criada', () => {
    
        cy.visit('http://localhost:4200/login');
        cy.get('#login-input-username').type('admin@email.pt');
        cy.get('#login-input-password').type('admin');
        cy.get('button[type="submit"]').click();
        cy.get('[id^="Tasks"]').click();
        cy.get('[routerlink="pending-task-list"]').click();

        cy.get('.mdc-data-table__content .cdk-column-user-name').last().should('contain', 'Cavalo Selvagem');
        cy.get('.mdc-data-table__content .cdk-column-description').last().should('contain', 'Testing Task 1');
        cy.get('.mdc-data-table__content .cdk-column-user-contact').last().should('contain', '651256455');


      });
 

  });