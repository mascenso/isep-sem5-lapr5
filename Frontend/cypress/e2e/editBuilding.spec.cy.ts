describe('Teste de editar um edificio', () => {
  
    beforeEach(() => {
        //criar edificio para editar
        cy.visit('http://localhost:4200/login');
        cy.get('#role').select('Administrador');
        cy.get('#signUp').click();
        cy.get('[id^="Campus"]').click();
        cy.get('[routerlink="create-building"]').click();
        const numeroAleatorio = Math.floor(1000 + Math.random() * 9000);
        cy.get('#mat-input-0').type('A'+numeroAleatorio.toString());
        cy.get('#mat-input-1').type('10');
        cy.get('#mat-input-2').type('50');
        cy.get('#mat-input-3').type('Edificio A');
        cy.get('#mat-input-4').type('Edificio A - Administracao');
        cy.get('.mdc-button__label').click();
        //ir para a pagina correta do teste
        cy.get('[id^="Campus"]').click();
        cy.get('[routerlink="edit-building"]').click();
    });
  
    
    it('Devem retornar 200 depois de fazer o patch.', () => {
        cy.intercept('PATCH', 'http://localhost:4000/api/buildings').as('rotaBuilding');

        cy.get('#buildingSelector').click().get('#mat-option-0').click();
        cy.get('#mat-input-6').clear().type('20');
        cy.get('#mat-input-6').clear().type('20');
        cy.get('#saveBuilding').click();

        cy.wait('@rotaBuilding').should(({ response }) => {
            expect(response!.statusCode).to.eq(200); 
        });
    });

  
  });