from django.shortcuts import render
from django.templatetags.static import static
from django.conf import settings
from django.shortcuts import redirect
from django.http import HttpResponse
from django.views.decorators.http import require_POST
from django.contrib import messages
from django.core.mail import EmailMessage
from django.shortcuts import redirect
from .models import Book
import logging
from django.views.decorators.clickjacking import xframe_options_exempt
from django.core.paginator import Paginator
from django.http import JsonResponse
from reportlab.lib.pagesizes import letter
from reportlab.pdfgen import canvas
from reportlab.lib import colors
import os

# def index(request):
#     print(request.user)
#     return render(request,'index.html') # Template name
def index(request):
    if not request.session.get('authenticated', False):
        return redirect('password_entry')
    return render(request, 'index.html')

def about(request):
    return render(request,'about.html') # Template name
def reports(request):
    return render(request,'reports.html')
def test(request):
    return render(request,'test.html') # Template name

def contact(request):
    return render(request,'contact.html') # Template name
@xframe_options_exempt
def read_pdf(request):
    pdf_url = request.GET.get('pdf_url', '')
    context = {'pdf_url': pdf_url}
    return render(request, 'read_pdf.html', context)  

logger = logging.getLogger(__name__)  # Set up a logger
@require_POST
def end_session_view(request):

    messages.success(request, "Your session has ended.")  # Add a success message
    return redirect('index')  # Redirect to 'index' named URL pattern

logger = logging.getLogger(__name__)  # Set up a logger

# def send_email(request):
#     user_email = request.POST.get('email')
#     try:
#         email = EmailMessage(
#             'Hello',
#             'Body goes here',
#             settings.EMAIL_HOST_USER,
#             [user_email],  # Replace with the recipient's email
#             reply_to=[settings.EMAIL_HOST_USER]
#             # Include attachments as needed
#         )
#         email.send()
#         messages.success(request, "Email sent successfully.")
#     except Exception as e:
#         # Log the error
#         logger.error(f'Error sending email: {e}')
#         # Add the error to messages
#         messages.error(request, f'Email could not be sent: {e}')
#     return redirect('index')  # Redirect to 'home' or any other page you wish

# def send_email(request):
#     if request.method == "POST":
#         user_email = request.POST.get('email')
#         try:
#             email = EmailMessage(
#                 'Session Ended',
#                 'Your session ended at ' + request.POST.get('end_time'),
#                 settings.EMAIL_HOST_USER,
#                 [user_email],
#                 reply_to=[settings.EMAIL_HOST_USER]
#             )
#             email.send()
#             return JsonResponse({"message": "Email sent successfully"}, status=200)
#         except Exception as e:
#             logger.error(f'Error sending email: {e}')
#             return JsonResponse({"error": str(e)}, status=500)
#     return JsonResponse({"error": "Invalid request"}, status=400)

# def create_pdf(data, filename):
#     c = canvas.Canvas(filename, pagesize=letter)
#     width, height = letter
#     c.drawString(100, 750, f"Book Read: {data['book']}")
#     c.drawString(100, 735, f"Time Elapsed: {data['time_elapsed']}")
#     c.save()

def create_pdf(data, filename):
    # Check if a previous report exists and remove it
    logo_path = os.path.join(settings.STATICFILES_DIRS[0], 'images/logo.png')
    # logo_path = os.path.join(settings.STATIC_ROOT, logo_url.lstrip('/'))
    if os.path.exists(filename):
        os.remove(filename)
        print("File removed")
    c = canvas.Canvas(filename, pagesize=letter)
    width, height = letter
    print("Inside new function")
    # Draw the image
 
    
    # Title and Intro
    c.setFont("Helvetica-Bold", 18)
    # c.drawCentredString(width / 2.0, height - 120, "MellowTales.tech")

    c.drawCentredString(width / 2.0, height - 120, "Engagement Report")


    c.drawImage(logo_path, 200, height - 100, width=50, height=50, mask='auto')

        # Draw a line
    c.setStrokeColor(colors.orange)
    c.line(50, height - 330, width - 50, height - 330)
    c.setFont("Helvetica", 12)
    c.drawString(50, height - 150, "This Engagement report has been generated using facial recognition")
    c.drawString(50, height - 165, "and eye tracking technology. The technology used captures the attention")
    c.drawString(50, height - 180, "score by tracking the eye movement engagement of the child when they are")
    c.drawString(50, height - 195, "focusing on the screen while reading the storybooks. The attention score is")
    c.drawString(50, height - 210, "the metric which is calculate by the formula:")

    # Formula Placeholder
    c.drawString(50, height - 240, "*Formula*")

    # Attention Score Generated
    c.setFont("Helvetica-Bold", 14)
    c.drawString(50, height - 270, "Attention Score Generated:")
    
    # Attention Score Details
    c.setFont("Helvetica", 12)
    c.drawString(50, height - 290, f"Name of the Book: {data['book']}")
    c.drawString(50, height - 305, f"Engagement Duration: {data['time_elapsed']}")
    c.drawString(50, height - 320, f"Attention Score: {data['attention_score']}")
    
    # Draw a line
    c.setStrokeColor(colors.orange)
    c.line(50, height - 330, width - 50, height - 330)

    # Recommendations
    c.setFont("Helvetica-Bold", 14)
    c.drawString(50, height - 360, "Recommendations:")
    
    c.setFont("Helvetica", 12)
    recommendations = [
        "Provide a balanced diet and routine",
        "Engage in physical activities and hobbies",
        "Teaching problem solving skills to your child in day to day life",
        "Increase concentration in your child by engaging in activities like puzzle, assisting with cooking, observing details in nature.",
        "Use a variety of novel and stimulating activities rather than boring worksheets.",
        "Create a quiet and organized study area free from distractions"
        # ... other recommendations
    ]

    y_position = height - 380
    for recommendation in recommendations:
        c.drawString(50, y_position, f"• {recommendation}")
        y_position -= 15

    # Save the PDF
    c.save()


def send_email(request):
    user_email = request.POST.get('email')
    book = request.POST.get('book_name')  # Assuming this is passed from the front-end
    time_elapsed = request.POST.get('time_elapsed')  # Assuming this is passed from the front-end
    
    # Generate PDF
    filename = 'SessionDetails.pdf'
    filepath = os.path.join(settings.MEDIA_ROOT, filename)
    # create_pdf({'book': book, 'time_elapsed': time_elapsed}, filepath)
    create_pdf({'book': book, 'time_elapsed': time_elapsed, 'attention_score': 1}, filepath)
    try:
        email = EmailMessage(
            'Session Ended',
            'Your session details are attached.',
            settings.EMAIL_HOST_USER,
            [user_email],
            reply_to=[settings.EMAIL_HOST_USER]
        )
        # Attach the PDF
        email.attach_file(filepath)
        
        email.send()
        return JsonResponse({"message": "Email sent successfully with attachment"}, status=200)
    except Exception as e:
        logger.error(f'Error sending email: {e}')
        return JsonResponse({"error": str(e)}, status=500)

def book_list(request):
    books = Book.objects.all()
    return render(request, 'books.html', {'books': books})


def password_entry(request):
    correct_password = "website123#"  # Set your password here

    if request.method == "POST":
        password = request.POST.get("password", "")
        if password == correct_password:
            request.session['authenticated'] = True  # Set session as authenticated
            return redirect('index')
        else:
            messages.error(request, 'Incorrect password, please try again.')
            return redirect('password_entry')

    return render(request, 'password_entry.html')